email(first_name,last_name,company) =
  "{String.lowercase(first_name)}.{String.lowercase(last_name)}@{company}.com"

// evaluates to "darth.vader@deathstar.com"
my_email = email("Darth","Vader","deathstar") 
