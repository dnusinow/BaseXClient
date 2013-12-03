# library("digest")                       # For digest/md5 
# library("stringr")                      # For str_trim

## BaseXSession Class
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
      writeBin(command, socket)
      last.result <<- readBin(socket, "character")
      last.info <<- readBin(socket, "character")
      if(! ok()) {
        stop(last.result)
      }
      return(last.result)
    },
    
    sendInput = function(code, arg, content) {
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
      BaseXQuery(.self, query)
    },
    
    create = function(dbname, content) {
      sendInput(8, dbname, content)
    },
    
    add = function(path, input) {
      sendInput(9, path, input)
    },
    
    replace = function(path, input) {
      sendInput(12, path, input)
    },
    
    store = function(path, input) {
      sendInput(session, 13, path, input)
    },
    
    info = function() {
      return(last.info)
    },
    
    ok = function() {
      return(as.raw(0) == readBin(socket, "raw"))
    },
    
    close = function() {
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
      session <<- sess
      id <<- exec(as.raw(0), qry)
      cache <<- new.env()
      cache$x <<- NULL
      callSuper(...)
    },
    
    finalize = function() {
      close()
    },
    
    exec = function(command, arg) {
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
      if(is.null(the.id)) {
        the.id <- id
      }
      exec(as.raw(5), the.id)
    },
    
    bind = function(vname, vval, datatype = "") {
      exec(as.raw(3), c(charToRaw(id), as.raw(0),
                        charToRaw(vname), as.raw(0),
                        charToRaw(vval), as.raw(0),
                        charToRaw(datatype)))
    },
    
    context = function(vval, datatype = "") {
      exec(as.raw(14), c(charToRaw(id), as.raw(0),
                         charToRaw(vval), as.raw(0),
                         charToRaw(datatype)))
    },
    
    qmore = function() {
      
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
      if(qmore()) {
        ret <- cache[["x"]][1]
        cache[["x"]] <<- cache[["x"]][-1]
        return(ret)
      } else {
        return(FALSE)
      }
    },
    
    info = function() {
      exec(as.raw(6), id)
    },
    
    options = function() {
      exec(as.raw(7), id)
    },
    
    updating = function() {
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