# If connection to mySQL db is open, close it
# disconnect to clean up the connection to the database.

if(user.writeDB == "y"){
  dbDisconnect(conDb)
}