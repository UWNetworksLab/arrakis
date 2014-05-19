#include <stdio.h>
#include <string.h>

#include <openssl/evp.h>

int print_hex(unsigned char *buf, int len)
{
	int i;
	int n;

	for (i = 0, n = 0; i < len; i++) {
		if (n > 7) {
			printf("\n");
			n = 0;
		}
		printf("0x%02x, ", buf[i]);
		n++;
	}
	printf("\n");

	return (0);
}


int main(int argc, char *argv[])
{
	char *pass = "password";
	char *salt = "1234";
	int ic = 1000;
	unsigned char key_material[256];

	unsigned char *key;
	unsigned char *iv;

	unsigned char inbuf[256];
	unsigned char outbuf[256];
	int outlen;
	int n;

	EVP_CIPHER_CTX ctx;

	PKCS5_PBKDF2_HMAC_SHA1(pass, strlen(pass), salt, strlen(salt), ic, 128, key_material);
	printf("PKCS5_PBKDF2_HMAC_SHA1(\"%s\", \"%s\", %d)=\n", pass, salt, ic);
	print_hex(key_material, 32);

	key = key_material;
	//iv = key_material + 8;
	//iv = key_material + 24;
	//iv = key_material + 16;
	//iv = key_material + 24;
	iv = key_material + 32;

	EVP_CIPHER_CTX_init(&ctx);
	//EVP_EncryptInit_ex(&ctx, EVP_des_cbc(), NULL, key, iv);
	//EVP_EncryptInit_ex(&ctx, EVP_des_ede3_cbc(), NULL, key, iv);
	//EVP_EncryptInit_ex(&ctx, EVP_aes_128_cbc(), NULL, key, iv);
	//EVP_EncryptInit_ex(&ctx, EVP_aes_192_cbc(), NULL, key, iv);
	EVP_EncryptInit_ex(&ctx, EVP_aes_256_cbc(), NULL, key, iv);

	printf("\ncipher parameters:\n");
	printf("    block size: %d\n", EVP_CIPHER_CTX_block_size(&ctx));
	printf("    key length: %d\n", EVP_CIPHER_CTX_key_length(&ctx));
	printf("     iv length: %d\n", EVP_CIPHER_CTX_iv_length(&ctx));

	while (1) {
		if ((n = fread(inbuf, 1, 128, stdin)) <= 0) {
			break;
		}
		if (!EVP_EncryptUpdate(&ctx, outbuf, &outlen, inbuf, n)) {
			return (0);
		}
		print_hex(outbuf, outlen);
	}

	if (!EVP_EncryptFinal_ex(&ctx, outbuf, &outlen)) {
		return (0);
	}
	print_hex(outbuf, outlen);

	EVP_CIPHER_CTX_cleanup(&ctx);

	return (0);
}

