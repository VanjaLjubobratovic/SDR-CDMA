rm(list=ls())
library(gtools)

source("./GoldCode.R")

# Definiranje kodova modulacije
codes <- generate_n_codes(c(25, 26, 27))
print(codes)

# učitavanje potrebnih poruka
data1 = asc(readLines(file("text0.txt", "r"))[1])
data2 = asc(readLines(file("text1.txt", "r"))[1])
data3 = asc(readLines(file("text2.txt", "r"))[1])


ascii_to_binary = function(msg) {
  # Konvertiranje ASCII vrijednosti u binarne vrijednosti
  binary = sapply(msg, function(x) {paste0(rev(as.integer(intToBits(x)[1:8])), collapse = "")})
  return(binary)
}

# Pretvaranje podataka iz ASCII u binarni
stream1 = ascii_to_binary(data1)
stream2 = ascii_to_binary(data2)
stream3 = ascii_to_binary(data3)


# Pronalazak najduže poruke kako bi se razlika duljine mogla popuniti
maxLen = max(length(stream1), length(stream2), length(stream3))

# Popunjavanje razlika u duljini s nulama
length(stream1) = maxLen
stream1[is.na(stream1)] = "00000000"

length(stream2) = maxLen
stream2[is.na(stream2)] = "00000000"

length(stream3) = maxLen
stream3[is.na(stream3)] = "00000000"

# Funkcija za modulacija poruka
code_message = function(stream, code) {
  coded_msg = c()
  code_neg = ifelse(code == 0, 1, ifelse(code == 1, 0, code))
  for(i in 1:length(stream)) {
    symbol = strsplit(stream[i], "")[[1]]
    for(j in 1:length(symbol)) {
      if(symbol[j] == "1"){
        coded_msg = c(coded_msg, code)
      } else {
        coded_msg = c(coded_msg, code_neg)
      }
    }
  }
  
  coded_msg = ifelse(coded_msg == 0, -1, 1)
  return(coded_msg)
}

# Moduliranje poruka
coded1 = code_message(stream1, code1)
coded2 = code_message(stream2, code2)
coded3 = code_message(stream3, code3)

# Funkcija za kompoziciju signala
composite_signal = function(coded1, coded2, coded3) {
  composite = coded1 + coded2 + coded3
  return(composite)
}

composite = composite_signal(coded1, coded2, coded3)

# Funkcija za demodulaciju
decode = function(code, composite) {
  message = c()
  for(i in seq(1, length(composite), 6)) {
    code_vect = composite[i:(i+5)]
    code_vect = code_vect * code
    code_sum = sum(code_vect)
    
    message = c(message, ifelse(code_sum < 0, 0, 1))
  }
  
  return(message)
}

# Demodulacija poruka
message1 = decode(code1, composite)
message2 = decode(code2, composite)
message3 = decode(code3, composite)

# Funkcija za pretvorbu binarnog koda u poruke
convert_to_string = function(message) {
  ascii_str = ""
  for(i in seq(1, length(message), 8)) {
    binary_str = message[i:(i+7)]
    binary_str = paste(binary_str, collapse="")
    decimal = strtoi(binary_str, base=2)
    
    ascii_str = paste(ascii_str, intToUtf8(decimal), sep="")
  }
  
  return(ascii_str)
}

# Prikaz demoduliranih poruka
print(convert_to_string(message1))
print(convert_to_string(message2))
print(convert_to_string(message3))