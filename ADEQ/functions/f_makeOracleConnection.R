makeConnection <- function(db,userId,passwd){
  tryCatch(
    {
      # message("This is the 'try' part")
      conn <- odbcConnect(db, uid=userId, pwd=passwd)
      #odbcGetInfo(conn)
      return(conn)
    } ,
    error = function(e){
      message("An error occurred:\n", e)
      return(FALSE)
    })
}