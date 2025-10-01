;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.db.embedded
  "Embedded PostgreSQL for local-first desktop mode.

   This namespace provides embedded PostgreSQL functionality for Kizu desktop app.
   Uses io.zonky.test:embedded-postgres to run PostgreSQL in-process.

   Usage:
     (start-embedded-postgres config)  ; Start embedded instance
     (stop-embedded-postgres)          ; Stop instance
     (get-connection-url)              ; Get JDBC URL
     (get-datasource)                  ; Get DataSource for connections"
  (:require
   [app.common.logging :as l]
   [clojure.java.io :as io])
  (:import
   [io.zonky.test.db.postgres.embedded EmbeddedPostgres
                                        EmbeddedPostgres$Builder]
   [java.nio.file Paths Files]
   [java.nio.file.attribute FileAttribute]))

(defonce ^:private embedded-instance
  "Atom holding the current embedded PostgreSQL instance"
  (atom nil))

(defn- ensure-data-directory
  "Ensure data directory exists and return Path object"
  [data-dir]
  (let [path (if (string? data-dir)
               (Paths/get data-dir (into-array String []))
               data-dir)]
    (when-not (Files/exists path (into-array java.nio.file.LinkOption []))
      (Files/createDirectories path (into-array FileAttribute [])))
    path))

(defn start-embedded-postgres
  "Start embedded PostgreSQL instance for local mode.

   Options:
     :data-dir - Directory for PostgreSQL data (required)
     :port     - Port to bind (default: 54321)

   Returns:
     EmbeddedPostgres instance

   Example:
     (start-embedded-postgres
       {:data-dir \"/path/to/data\"
        :port 54321})"
  [{:keys [data-dir port] :or {port 54321}}]
  (when @embedded-instance
    (l/warn :hint "Embedded PostgreSQL already running, stopping first")
    (stop-embedded-postgres))

  (l/info :hint "Starting embedded PostgreSQL"
          :data-dir data-dir
          :port port)

  (try
    (let [data-path (ensure-data-directory data-dir)
          builder (EmbeddedPostgres/builder)]

      ;; Configure builder
      (.setDataDirectory builder data-path)
      (.setPort builder port)

      ;; Start PostgreSQL
      (let [pg (.start builder)]
        (reset! embedded-instance pg)
        (l/info :hint "Embedded PostgreSQL started successfully"
                :port (.getPort pg)
                :data-dir (str data-path))
        pg))

    (catch Exception e
      (l/error :hint "Failed to start embedded PostgreSQL"
               :cause e)
      (throw e))))

(defn stop-embedded-postgres
  "Stop embedded PostgreSQL instance.

   Safe to call multiple times - does nothing if not running."
  []
  (when-let [pg @embedded-instance]
    (l/info :hint "Stopping embedded PostgreSQL")
    (try
      (.close pg)
      (l/info :hint "Embedded PostgreSQL stopped successfully")
      (catch Exception e
        (l/error :hint "Error stopping embedded PostgreSQL"
                 :cause e))
      (finally
        (reset! embedded-instance nil)))))

(defn get-connection-url
  "Get JDBC connection URL for embedded PostgreSQL instance.

   Returns:
     JDBC URL string (e.g., 'jdbc:postgresql://localhost:54321/postgres')

   Throws:
     Exception if embedded PostgreSQL not running"
  []
  (if-let [pg @embedded-instance]
    (str "jdbc:postgresql://localhost:" (.getPort pg) "/postgres")
    (throw (ex-info "Embedded PostgreSQL not running"
                    {:hint "Call start-embedded-postgres first"}))))

(defn get-datasource
  "Get javax.sql.DataSource for embedded PostgreSQL instance.

   Returns:
     DataSource instance for making connections

   Throws:
     Exception if embedded PostgreSQL not running"
  []
  (if-let [pg @embedded-instance]
    (.getPostgresDatabase pg)
    (throw (ex-info "Embedded PostgreSQL not running"
                    {:hint "Call start-embedded-postgres first"}))))

(defn get-port
  "Get port number of running embedded PostgreSQL instance.

   Returns:
     Port number (e.g., 54321) or nil if not running"
  []
  (when-let [pg @embedded-instance]
    (.getPort pg)))

(defn running?
  "Check if embedded PostgreSQL instance is currently running.

   Returns:
     true if running, false otherwise"
  []
  (some? @embedded-instance))

(defn get-database-info
  "Get information about running embedded PostgreSQL instance.

   Returns:
     Map with :running?, :port, :connection-url
     Or nil if not running"
  []
  (when-let [pg @embedded-instance]
    {:running? true
     :port (.getPort pg)
     :connection-url (get-connection-url)
     :instance pg}))

(comment
  ;; REPL usage examples

  ;; Start embedded PostgreSQL
  (start-embedded-postgres
   {:data-dir "/tmp/kizu-test-db"
    :port 54321})

  ;; Check status
  (running?)
  (get-database-info)

  ;; Get connection
  (def ds (get-datasource))
  (def conn (.getConnection ds))

  ;; Use connection
  ;; (... query database ...)

  ;; Cleanup
  (.close conn)
  (stop-embedded-postgres)
  )
