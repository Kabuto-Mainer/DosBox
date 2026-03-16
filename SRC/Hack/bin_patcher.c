#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>


static const char *NAME_PROGRAM_FILE    = "PSW.asm";
static const char *NAME_PATCH_FILE      = "HACK.asm";

static const int ADDRESS_REPLACE_PATCH[] = {1, 2};
static const int OFFSET_PATCH = 100;





int main() {
    size_t size_program = get_file_size(NAME_PROGRAM_FILE);
    size_t size_patch = get_file_size(NAME_PROGRAM_FILE);

    char *buffer = create_char_buffer(size_program + size_patch);

    FILE *stream = fopen(NAME_PROGRAM_FILE, "rb");
    fread(buffer, size_program, sizeof(char), stream);
    fclose(stream);

    stream = fopen(NAME_PATCH_FILE, "rb");
    fread(buffer + size_program, size_patch, sizeof(char), stream);
    fclose(stream);

    for (int i = 0; i < )
}







// ---------------------------------------------------------------------------------------------------
/**
 @brief Функция загрузки содержимого файла в буфер
 @param [in] name_file Имя загружаемого файла
 @return Указатель на выделенную динамически память
*/
char * create_char_buffer(size_t size) {
    char *buffer = (char *)calloc(sizeof(char), size);
    if (buffer == NULL) {
        printf("NULL calloc\n");
    }

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
