#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>


static const char *NAME_PROGRAM_FILE    = "PSW.asm";
static const char *NAME_PATCH_FILE      = "HACK.asm";

static const int32_t ADDRESS_REPLACE_PATCH[] = {1, 2};
static const int OFFSET_PATCH = 100;





int main() {
    size_t size_program = get_file_size(NAME_PROGRAM_FILE);
    size_t size_patch = get_file_size(NAME_PATCH_FILE);

}







// ---------------------------------------------------------------------------------------------------
/**
 @brief Функция загрузки содержимого файла в буфер
 @param [in] name_file Имя загружаемого файла
 @return Указатель на выделенную динамически память
*/
char * create_char_buffer(const char *file_name) {
    assert(file_name);

    size_t size = get_file_size(file_name);
    char *buffer = (char *)calloc(1, size + 1);
    if (buffer == NULL) {
        printf("ERROR with CALLOC\n");
        return NULL;
    }

    FILE *stream = fopen(file_name, "rb");
    if (stream == NULL) {
        printf("NULL file\n");
        free(buffer);
        return NULL;
    }

    size_t amount_chars = fread(buffer, sizeof(char), size, stream);
    fclose(stream);
    buffer[amount_chars] = '\0';

    return buffer;
}

// -------------------------------------------------------------------------------------------------------
/**
 * @brief Функция, находящая размер файла в байтах по его имени
 * @param [in] name_file Имя файла
 * @return Размер файла
*/
size_t get_file_size(const char* name_file) {
    assert(name_file);

    struct stat file_s = {};

    if (stat(name_file, &file_s) == -1) {
        printf ("ERROR with stat\n");
        return 0;
    }

    return (size_t) file_s.st_size;
}
// -------------------------------------------------------------------------------------------------------
