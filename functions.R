# Copyright (c) 2014 Oliver Keyes
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

#SQL-queryin' function
rawsql <- function(statement){
  
  #Open connection to the MySQL DB
  con <- dbConnect(drv = "MySQL",
                   username = db_user,
                   password = db_pass,
                   host = db_host,
                   dbname = db_database)
  
  QuerySend <- dbSendQuery(con, statement)
  
  #Retrieve output of query
  output <- fetch(QuerySend, n = -1)
  
  #Kill connection
  dbDisconnect(con)
  
  #Return
  return(output)
  
}

#Global querying function
global_query <- function(serverlist, wikilist, statement){
  
  #Generate query results
  query_results.ls <- lapply(serverlist, function(x){
    
    #For each server, find out what the available dbs are
    available.dbs <- rawsql(statement = "SHOW DATABASES;", db_host = x)
    
    #Filter the available dbs to active wikis
    available.dbs <- available.dbs$Database[available.dbs$Database %in% wikilist]
    
    #Instantiate output object.
    output.ls <- list()
    
    #For each available DB on that slave, run the query
    for(i in seq_along(available.dbs)){
      
      #Retrieve data, add the DB and bind both into the output object
      output.ls[[i]] <- cbind(rawsql(statement = statement,
                                     db_host = x,
                                     db_database = available.dbs[i])
                              ,available.dbs[i])
      
    }
    
    #Turn the results into a single data frame
    output.df <- do.call("rbind",output.ls)
    
    #Return
    return(output.df)
  })
  
  #Turn into a single data frame
  query_results.df <- do.call("rbind",query_results.ls)
  
  #Return
  return(query_results.df)
}
