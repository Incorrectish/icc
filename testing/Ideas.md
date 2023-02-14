# How to make new user-defined types work? We can have a set of types, both
# introduced within the project and already created, and then check if it in
# within, it might require some changes to our token type, but that can be
# figured out, we could first go through the files, and find our struct
# definitions, then add that to the "Keyword Set", and check if an element is
# within whenever we encounter one. Probably the most readable and scalable
# solution
