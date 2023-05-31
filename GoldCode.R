rm(list=ls()) 


int_xor <- function(bit1, bit2) {
  return (abs(bit1 - bit2))
}

generate_gold_code <- function(seed){
  # Generates codes up to 127 bits
  g1 <- c(1, 1, 1, 0, 0, 0, 1)
  g2 <- c(0, 0, 1, 0, 0, 0, 1)
  last <- length(g1)
  
  # Bit number 1 is left, bit number 7 is right
  r1 <- c(0, 0, 0, 0, 0, 0, 1)
  r2 <- seed
  
  gold_code <- c()
  
  for(i in 1:20) {
    # Compute XOR of last bits in shift registers and output it
    output_bit <- int_xor(r1[last], r2[last])
    
    in_r1 = NA
    in_r2 = NA
    
    # Calculate new input bits via prefered pairs
    for(j in seq(last, 1, -1)) {
      if(g1[j] == 1) {
        if(is.na(in_r1)) {
          in_r1 <- r1[j]
        } else {
          in_r1 <- int_xor(in_r1, r1[j])
        }
      }
      
      if(g2[j] == 1) {
        if(is.na(in_r2)) {
          in_r2 <- r2[j]
        } else {
          in_r2 <- int_xor(in_r2, r2[j])
        }
      }
    }
    
    # Shift registers
    r1 <- c(in_r1, r1[1:last-1])
    r2 <- c(in_r2, r2[1:last-1])
    
    # Store new output
    gold_code <- c(gold_code, output_bit)
  }
  
  return (gold_code)
}

generate_n_codes <- function(seeds) {
  codes <- list()
  for(i in 1:length(seeds)) {
    # Turn int to binary array
    seed_bin <- integer(7);
    seed_bin[1:7] <- as.integer(intToBits(seeds[i]))[1:7]
    
    codes[[i]] <- generate_gold_code(seed_bin)
  }
  
  return (codes)
}


codes <- generate_n_codes(c(25, 26, 27))
print(codes)

