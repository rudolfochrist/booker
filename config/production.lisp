
(in-package #:booker)

(setf (address *config*) (merge-pathnames ".local/var/sock/booker.sock"
                                          (user-homedir-pathname))
      (port *config*) 45000
      (database-name *config*) "booker_production"
      (database-host *config*) :unix
      (database-port *config*) 15432
      (access-log-destination *config*) (merge-pathnames ".local/var/log/booker.log"
                                                         (user-homedir-pathname))
      (message-log-destination *config*) (merge-pathnames ".local/var/log/booker.log"
                                                          (user-homedir-pathname)))
