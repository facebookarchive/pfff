// expressions can be embedded into strings between curly braces
x = "1 + 2 is {1+2}" 
// evaluates to "1 + 2 is 3"

email(first_name,last_name,company) =
  "{String.lowercase(first_name)}.{String.lowercase(last_name)}@{company}.com"

// evaluates to "darth.vader@deathstar.com"
my_email = email("Darth","Vader","deathstar") 
