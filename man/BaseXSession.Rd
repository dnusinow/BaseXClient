\name{BaseXSession}
\alias{BaseXSession}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Client Interface To the BaseX Server}
\description{
BaseXSession generates an object that can be used to send commands, queries, and data to a BaseX server. The object implements the standard client interface used to access BaseX by other languages. 
}
\usage{
BaseXSession(user, pass, host = "localhost", port = 1984, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user} { The BaseX username }
  \item{pass} { The BaseX user's password }
  \item{host} { The URL to access the BaseX server }
  \item{port} { What port the BaseX server is listening on }
  \item{\dots}{ Any additional arguments. Passed to superclass constructor }
}
\details{
\code{BaseXSession} will attempt to connect to the BaseX server and if it succeeds it will return a
\code{BaseXSession} object that can be used to interact with the server over that connection.
}
\value{
A \code{BaseXSession} object that has methods to interact with the server. Internally, this is implemented as a reference object (rather than the more common S3 and S4 objects) and so has certain distinctive properties. Because the connection to the server is stateful, the object uses call by reference rather than by value. This means that writing functional code that returns the BaseXSession object is not necessary and you can simply keep making method calls on the same object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
David Nusinow <david@gravitypulls.net>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (...) 
new("BaseXSession", ...), className = structure("BaseXSession", package = "BaseXClient"), package = "BaseXClient", generator = <S4 object of class structure("refGeneratorSlot", package = "methods")>, class = structure("refObjectGenerator", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
