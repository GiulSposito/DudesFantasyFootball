# library(plumber)

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  list(msg=paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @png
#* @get /plot
function(){
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param a the first number to add
#* @param b the second number to add
#* @post /sum
function(a,b){
  as.numeric(a) + as.numeric(b)
}
