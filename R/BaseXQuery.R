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