#' Session class for BaseX
#' 
#' \code{BaseXSession} creates a session object that can be used to communicate with the BaseX server.
#' 
#' @param user The BaseX database username
#' @param pass The BaseX user password
#' @param host The hostname of the machine running the BaseX server
#' @param port The port that the BaseX server is listening on
#' @param ... Any additional parameters to pass to the superclass constructor
BaseXSession <- setRefClass(
  Class = "BaseXSession",
  fields = c("socket", 
             "host", 
             "port", 
             "user", 
             "timestamp", 
             "last.info", 
             "last.result"),
  
  methods = list(

    initialize = function(user, pass, host = "localhost", port = 1984, ...) {
      host <<- host
      user <<- user
      socket <<- socketConnection(host = host, port = port, server = FALSE,
                                  blocking = TRUE, open = "r+b",
                                  encoding = getOption("encoding"),
                                  timeout = getOption("timeout"))
      timestamp <<- readBin(socket, "character")
      
      writeBin(user, socket)
      writeBin(digest(paste0(digest(pass, algo = "md5", serialize = FALSE),
                             timestamp),
                      algo = "md5", serialize = FALSE), 
               socket)
      resp <- readBin(socket, "raw", size = 1)
      if(any(c(length(resp) != 1), 
             (resp != as.raw(0)))) {
        stop("Access denied")
      }
      
      callSuper(...)
    },
    
    finalize = function() {
      close()
    },
    
    execute = function(command) {
      "Sends the character string specified by {command} to the server and returns the result."
      writeBin(command, socket)
      last.result <<- readBin(socket, "character")
      last.info <<- readBin(socket, "character")
      if(! ok()) {
        stop(last.result)
      }
      return(last.result)
    },
    
    sendInput = function(code, arg, content) {
      "Internal function used to send common commands. Should not be called by external code."
      command <- c(as.raw(code), 
                   charToRaw(arg), as.raw(0),
                   charToRaw(content), as.raw(0))
      writeBin(command, socket)
      last.result <<- readBin(socket, "character")
      if( !ok() ) {
        stop(last.result)
      }
      return(last.result)
    },
    
    query = function(query) {
      "Creates a BaseXQuery object using the {query} argument string."
      BaseXQuery(.self, query)
    },
    
    create = function(dbname, content) {
      "Creates a new database using the name {dbname} 
      and the character string {content}. {content} may be empty."
      sendInput(8, dbname, content)
    },
    
    add = function(path, input) {
      "Adds the document {input} to the database specified by {path}."
      sendInput(9, path, input)
    },
    
    replace = function(path, input) {
      "Replaces the document at {path} with the content in {input}."
      sendInput(12, path, input)
    },
    
    store = function(path, input) {
      "Stores the {input} content at the database location {path}."
      sendInput(session, 13, path, input)
    },
    
    info = function() {
      "Returns the value of the last interaction."
      return(last.info)
    },
    
    ok = function() {
      "Internal function that should not be called by extrnal code."
      return(as.raw(0) == readBin(socket, "raw"))
    },
    
    close = function() {
      "Close the connection. This is automatically called when the object is garbage collected."
      writeBin("exit", socket)
      close.connection(socket)
      ## rm(socket)
    })
)

## BaseXSession test code
# foo <- BaseXSession(user = "admin", pass = "admin", host = "localhost")
# foo$execute("INFO")
# foo$create("notedb", "")
# myxml <- do.call("paste0", as.list(readLines("~/note.xml")))
# foo$add("notedb", myxml)
# foo$execute("drop db notedb")
# foo$close()
# rm(foo)
