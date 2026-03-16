getRoot <- function(type, dns) {
  dns <- tolower(dns)
  type <- tolower(type)
  
  roots <- campRoots[[dns]]
  
  if (type == "especies") {
    if (dns == "arsa")
      return(roots$arsa)
    else
      return(roots$especies)
  }
  
  return(roots[[type]])
}
