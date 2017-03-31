# Install required packages as necesssary
required_packages = c("RODBC", "data.table", "plyr", "ggplot2", "scales", "lazyeval")
for(package in required_packages) {
    if(!(package %in% installed.packages()[,"Package"])) install.packages(package)  
}
