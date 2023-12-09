-- basic idea: massive data size makes lookup difficult
-- so instead we use a bloom filter to check versus some cheap hash to not waste lookups
-- exact details depending on harry's feedback on Friday
-- eg datatype
-- smallHashableValue : error "this is too large to mess with directly"

-- can just use a pulled dataset
-- use a library to parse
-- called cassava

-- final idea: murmur hash along with a list of usernames/passwords to see which ones have been used already