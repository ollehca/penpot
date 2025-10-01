;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.db-embedded-test
  "Tests for embedded PostgreSQL functionality"
  (:require
   [clojure.test :refer :all]
   [app.db.embedded :as embedded])
  (:import
   [java.sql Connection Statement ResultSet]))

(def test-config
  {:data-dir "/tmp/kizu-embedded-pg-test"
   :port 54399})

(defn cleanup-test-db
  "Clean up test database"
  [f]
  (try
    ;; Stop any running instance
    (when (embedded/running?)
      (embedded/stop-embedded-postgres))

    ;; Run tests
    (f)

    (finally
      ;; Always cleanup
      (when (embedded/running?)
        (embedded/stop-embedded-postgres)))))

(use-fixtures :once cleanup-test-db)

(deftest test-start-stop-embedded-postgres
  (testing "Can start embedded PostgreSQL"
    (let [pg (embedded/start-embedded-postgres test-config)]
      (is (some? pg) "PostgreSQL instance should be created")
      (is (embedded/running?) "PostgreSQL should be running")
      (is (= 54399 (embedded/get-port)) "Port should match config")))

  (testing "Can stop embedded PostgreSQL"
    (embedded/stop-embedded-postgres)
    (is (not (embedded/running?)) "PostgreSQL should be stopped")))

(deftest test-connection-url
  (testing "Can get connection URL"
    (embedded/start-embedded-postgres test-config)
    (let [url (embedded/get-connection-url)]
      (is (string? url) "URL should be a string")
      (is (re-find #"jdbc:postgresql://localhost:54399/postgres" url)
          "URL should contain correct connection string"))
    (embedded/stop-embedded-postgres)))

(deftest test-datasource
  (testing "Can get DataSource"
    (embedded/start-embedded-postgres test-config)
    (let [ds (embedded/get-datasource)]
      (is (some? ds) "DataSource should not be nil")
      (is (instance? javax.sql.DataSource ds)
          "Should be a DataSource instance"))
    (embedded/stop-embedded-postgres)))

(deftest test-database-connection
  (testing "Can connect to embedded database"
    (embedded/start-embedded-postgres test-config)

    (let [ds (embedded/get-datasource)
          conn (.getConnection ds)]
      (is (some? conn) "Connection should be created")
      (is (instance? Connection conn) "Should be a Connection instance")
      (is (not (.isClosed conn)) "Connection should be open")

      ;; Test basic query
      (testing "Can execute queries"
        (let [stmt (.createStatement conn)
              rs (.executeQuery stmt "SELECT 1 AS test")]
          (is (.next rs) "Result set should have data")
          (is (= 1 (.getInt rs "test")) "Query should return correct value")
          (.close rs)
          (.close stmt)))

      (.close conn))

    (embedded/stop-embedded-postgres)))

(deftest test-create-table-and-insert
  (testing "Can create tables and insert data"
    (embedded/start-embedded-postgres test-config)

    (let [ds (embedded/get-datasource)
          conn (.getConnection ds)]
      (try
        ;; Create test table
        (with-open [stmt (.createStatement conn)]
          (.execute stmt
                    "CREATE TABLE test_users (
                       id SERIAL PRIMARY KEY,
                       name VARCHAR(100) NOT NULL,
                       email VARCHAR(200) UNIQUE NOT NULL
                     )"))

        ;; Insert data
        (with-open [stmt (.createStatement conn)]
          (.executeUpdate stmt
                          "INSERT INTO test_users (name, email) VALUES
                           ('Alice', 'alice@example.com'),
                           ('Bob', 'bob@example.com')"))

        ;; Query data
        (with-open [stmt (.createStatement conn)
                    rs (.executeQuery stmt
                                      "SELECT COUNT(*) as count
                                       FROM test_users")]
          (is (.next rs) "Should have results")
          (is (= 2 (.getInt rs "count"))
              "Should have 2 users"))

        ;; Query specific user
        (with-open [stmt (.createStatement conn)
                    rs (.executeQuery stmt
                                      "SELECT name, email FROM test_users
                                       WHERE name = 'Alice'")]
          (is (.next rs) "Should find Alice")
          (is (= "Alice" (.getString rs "name")))
          (is (= "alice@example.com" (.getString rs "email"))))

        (finally
          (.close conn))))

    (embedded/stop-embedded-postgres)))

(deftest test-postgresql-features
  (testing "PostgreSQL-specific features work"
    (embedded/start-embedded-postgres test-config)

    (let [ds (embedded/get-datasource)
          conn (.getConnection ds)]
      (try
        ;; Test UUID support
        (testing "UUID type"
          (with-open [stmt (.createStatement conn)]
            (.execute stmt
                      "CREATE TABLE test_uuids (
                         id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
                         name VARCHAR(100)
                       )"))

          (with-open [stmt (.createStatement conn)]
            (.executeUpdate stmt
                            "INSERT INTO test_uuids (name) VALUES ('test')"))

          (with-open [stmt (.createStatement conn)
                      rs (.executeQuery stmt
                                        "SELECT id FROM test_uuids")]
            (is (.next rs) "Should have UUID record")
            (is (some? (.getString rs "id")) "UUID should be generated")))

        ;; Test JSONB support
        (testing "JSONB type"
          (with-open [stmt (.createStatement conn)]
            (.execute stmt
                      "CREATE TABLE test_json (
                         id SERIAL PRIMARY KEY,
                         data JSONB
                       )"))

          (with-open [stmt (.createStatement conn)]
            (.executeUpdate stmt
                            "INSERT INTO test_json (data) VALUES
                             ('{\"name\": \"test\", \"value\": 123}')"))

          (with-open [stmt (.createStatement conn)
                      rs (.executeQuery stmt
                                        "SELECT data->>'name' AS name
                                         FROM test_json")]
            (is (.next rs) "Should have JSONB record")
            (is (= "test" (.getString rs "name"))
                "JSONB query should work")))

        (finally
          (.close conn))))

    (embedded/stop-embedded-postgres)))

(deftest test-database-info
  (testing "Can get database info"
    (embedded/start-embedded-postgres test-config)

    (let [info (embedded/get-database-info)]
      (is (map? info) "Info should be a map")
      (is (:running? info) "Should show as running")
      (is (= 54399 (:port info)) "Port should be correct")
      (is (string? (:connection-url info)) "URL should be present")
      (is (some? (:instance info)) "Instance should be present"))

    (embedded/stop-embedded-postgres)

    (is (nil? (embedded/get-database-info))
        "Info should be nil when stopped")))

(deftest test-error-handling
  (testing "Error when getting URL without starting"
    (is (not (embedded/running?)) "Should not be running")
    (is (thrown? Exception (embedded/get-connection-url))
        "Should throw when not running"))

  (testing "Error when getting DataSource without starting"
    (is (thrown? Exception (embedded/get-datasource))
        "Should throw when not running")))

(deftest test-restart
  (testing "Can restart embedded PostgreSQL"
    ;; Start first time
    (embedded/start-embedded-postgres test-config)
    (is (embedded/running?) "Should be running")
    (let [first-port (embedded/get-port)]
      (is (= 54399 first-port)))

    ;; Stop
    (embedded/stop-embedded-postgres)
    (is (not (embedded/running?)) "Should be stopped")

    ;; Start again
    (embedded/start-embedded-postgres test-config)
    (is (embedded/running?) "Should be running again")
    (let [second-port (embedded/get-port)]
      (is (= 54399 second-port) "Port should be same"))

    ;; Cleanup
    (embedded/stop-embedded-postgres)))

(comment
  ;; Run all tests
  (run-tests)

  ;; Run specific test
  (test-start-stop-embedded-postgres)
  (test-database-connection)
  (test-create-table-and-insert)
  (test-postgresql-features)
  )
