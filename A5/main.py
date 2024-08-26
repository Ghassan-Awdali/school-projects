def modular_pow(base, exponent, modulus):
    result = 1
    while exponent > 0:
        if exponent % 2 == 1:
            result = (result * base) % modulus
        exponent = exponent // 2
        base = (base * base) % modulus
    return result

p = int(input("Enter a prime number for p: "))
q = int(input("Enter a prime number for q: "))
n = p*q
minusone = (p-1)*(q-1)

for i in range(2,n):
    if minusone%i != 0:
        e = i
        break

findD = True

check = minusone+1

while check % minusone == 1:
    if(check%e == 0):
        d = int(check/e)
        break
    check+= minusone

print("Calculating RSA values")
print("Public RSA key is ({}, {})". format(e, n))
print("Public RSA key is ({}, {})". format(d, n))

m = int(input("Enter the plaintext message m (an integer)"))

c = modular_pow(m,e,n)


print("the cipher text c is {}".format(c))

print("decrypting c...")
print("The plaintext m is {}".format(modular_pow(c,d,n)))




