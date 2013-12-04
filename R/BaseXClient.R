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

## BaseXQuery Class
BaseXQuery <- setRefClass(
  Class = "BaseXQuery",
  fields = c("session", 
             "id",
             "cache"),
  
  methods = list(
    
    initialize = function(sess, qry, ...) {
      "Creates a query using the {sess} BaseXSession object and {qry} character string."
      session <<- sess
      id <<- exec(as.raw(0), qry)
      cache <<- new.env()
      cache$x <<- NULL
      callSuper(...)
    },
    
    finalize = function() {
      "Internal destructor function. User code should call the object's close() method instead."
      close()
    },
    
    exec = function(command, arg) {
      "Internal function. User code should call the object's execute() method to execute the query."
      if(!is.raw(arg)) {
        arg <- charToRaw(arg)
        ##print(arg)
      }
      writeBin(c(as.raw(command), arg, as.raw(0)), session$socket)
      resp <- readBin(session$socket, "character")
      ##print(resp)
      status <- readBin(session$socket, "raw")
      ##print(status)
      if(status != as.raw(0)) {
        stop(readBin(session$socket, "character"))
      }
      return(resp)
    },
    
    execute = function(the.id = NULL) {
      "Execute the query and return the result."
      if(is.null(the.id)) {
        the.id <- id
      }
      exec(as.raw(5), the.id)
    },
    
    bind = function(vname, vval, datatype = "") {
      "Binds variable {vname} to have the value {vval} in the query string."
      exec(as.raw(3), c(charToRaw(id), as.raw(0),
                        charToRaw(vname), as.raw(0),
                        charToRaw(vval), as.raw(0),
                        charToRaw(datatype)))
    },
    
    context = function(vval, datatype = "") {
      "Binds a value {vval} to the context item"
      exec(as.raw(14), c(charToRaw(id), as.raw(0),
                         charToRaw(vval), as.raw(0),
                         charToRaw(datatype)))
    },
    
    qmore = function() {
      "Implements the standard more() method for other clients. Checks to see if there are 
      more results pending from the query."
      
      if(is.null(cache$x) ) {
        
        ## Utility functions start
        read.until <- function(con, b) {
          raw.vec <- vector(mode = "raw")
          while(TRUE) {
            cur.raw <- readBin(con, "raw")
            if(cur.raw == b) {
              return(raw.vec)
            }
            raw.vec <- c(raw.vec, cur.raw)
          }
        }
        read.string <- function(con) {
          rawToChar(read.until(con, as.raw(0)))
        }
        ## Utility functions end
        
        writeBin(c(as.raw(4), charToRaw(id), as.raw(0)), session$socket)
        cache$x <<- vector(mode = "raw")
        
        while(! session$ok() ) {
          cache$x <<- c(cache$x, read.string(session$socket))
          ##print(cache$x)
        }
        
        if( ! session$ok() ) {
          return(readBin(session$socket, "character"))
        }
      }
      
      if(length(cache$x) > 0) {
        return(TRUE)
      } else {
        cache$x <<- NULL
        return(FALSE)
      }
    },
    
    qnext = function() {
      "Implements the standard next() method for other clients. 
      Returns the next pending result from the query."
      if(qmore()) {
        ret <- cache[["x"]][1]
        cache[["x"]] <<- cache[["x"]][-1]
        return(ret)
      } else {
        return(FALSE)
      }
    },
    
    info = function() {
      "Returns a character vector with information about compilation of the query."
      exec(as.raw(6), id)
    },
    
    options = function() {
      "Returns a character vector with information about serialization options."
      exec(as.raw(7), id)
    },
    
    updating = function() {
      "Checks if the query may perform updates. Returns TRUE if so and FALSE if not."
      ret <- exec(as.raw(30), id)
      if(ret == "true") {
        return(TRUE)
      }
      if(ret == "false") {
        return(FALSE)
      }
      else {
        stop("Don't understand return value of ", ret)
      }
    },
    
    close = function() {
      "Closes the query and de-registers it with the server. Should be called by the client
      but is also called automatically when the query is garbage collected."
      exec(as.raw(2), id)
    })
)

## BaseXQuery Test Code
# foo <- BaseXSession(user = "admin", pass = "admin", host = "localhost")
# bar <- foo$query("1 + 10")
# bar$execute()
# bar$options()
# bar$info()
# bar$close()
# rm(bar)
# 
# baz <- BaseXQuery(foo, "2 + 2")
# baz$execute()
# rm(baz)
# 
# baz <- BaseXQuery(foo, "declare variable $name external; for $i in 1 to 10 return element { $name } { $i }")
# baz$bind("name", "number")
# baz$execute()
# baz$updating()
# baz$close()
# rm(baz)
#  
# quux <- BaseXQuery(foo, "declare variable $name external; for $i in 1 to 2 return element { $name } { $i }")
# quux$bind("name", "number")
# while(quux$qmore()) {
#   print(quux$qnext())
# }
# quux$close()
# rm(quux)
# 
# foo$close()
# rm(foo)