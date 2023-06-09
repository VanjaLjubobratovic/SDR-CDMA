rm(list=ls())


source("./GoldCode.R")
codes <- generate_n_codes(c(25, 26, 27))

# Definiranje kodova modulacije
len = 20

# učitavanje potrebnih poruka
data = list(asc(readLines(file("text0.txt", "r"))[1]),asc(readLines(file("text1.txt", "r"))[1])
            ,asc(readLines(file("text2.txt", "r"))[1]))

# Pronalazak najduže poruke i postavljanje svih poruka na tu dužinu  
maxLen = 0
for(d in data){
  if(length(d) > maxLen){
    maxLen = length(d)
  }
}

for(i in 1:length(data)){
  length(data[[i]]) = maxLen
}


ascii_to_binary = function(msg) {
  # Konvertiranje ASCII vrijednosti u binarne vrijednosti
  binary = sapply(msg, function(x) {paste0(rev(as.integer(intToBits(x)[1:8])), collapse = "")})
  return(binary)
}

# Pretvaranje podataka iz ASCII u binarni

stream <- list()
k = 1
for(d in data){
  stream[[k]] <- ascii_to_binary(d)
  k = k + 1
}

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
coded = list()
k = 1
for(s in stream){
  coded[[k]] = code_message(s, codes[[k]])
  k = k + 1
}
#Ispis dijela kodirane poruke za prikaz izgleda
coded[[1]][1:30]

# Funkcija za kompoziciju signala
composite_signal = function(coded) {
  composite = 0
  for(c in coded){
    composite = composite + c
  }
  return(composite)
}

composite = composite_signal(coded)
#Ispis kompozitne poruke za prikaz izgleda
composite[1:30]

# Funkcija za demodulaciju
decode = function(code, composite) {
  message = c()
  for(i in seq(1, length(composite), len)) {
    code_vect = composite[i:(i+len-1)]
    code_vect = code_vect * code
    code_sum = sum(code_vect)
    
    message = c(message, ifelse(code_sum < 0, 0, 1))
  }
  
  return(message)
}

# Demodulacija poruka
messages = list()
k = 1
for(c in codes){
  messages[[k]] = decode(c, composite)
  k = k +1
}

#Definiranje novog koda kako bi se pokušala pročitati poruka
fakeCode = generate_n_codes(10)
fakeMessage = decode(fakeCode[[1]], composite)

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
for(m in messages){
  print(convert_to_string(m))
}

#Primjer dekodiranja poruke s "krivim" kodom
print(convert_to_string(fakeMessage))
