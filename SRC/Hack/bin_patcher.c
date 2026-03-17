#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <assert.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_mixer.h>

// Exit functions with auto print error
// --------------------------------------------------------------------------------------------------
#define EXIT_FUNC(reason, return_value) \
 printf ("ERROR in \"%s:%d\": %s\n", __FILE__, __LINE__, reason); \
 return return_value;
// --------------------------------------------------------------------------------------------------


typedef int OBJECT_ID;

typedef struct text_obj {
    SDL_Rect place;
    SDL_Texture *tex;
} text_obj;

typedef struct button_obj {
    SDL_Rect place;

    SDL_Texture *pr;
    SDL_Texture *un;
} button_obj;

typedef enum object_kind {
    TEXT_OBJECT,
    BUTTON_OBJECT,
} object_kind;

typedef struct object_entry {
    object_kind kind;

    union {
        text_obj text;
        button_obj but;
    } data;
} object_entry;

typedef struct object_collection {
    int size;
    OBJECT_ID *pool;
} object_collection;

typedef struct display {
    SDL_Window *win;
    SDL_Renderer *ren;
    TTF_Font *font;

    object_entry pool[SIZE_POOL];
    struct {
        object_collection *collect;
        int amount;
    } menu;
} display;


typedef enum EVENT {
    NULL_EVENT,
    START_PROGRAM,
    PATCH_FILE,
    CONTROL_MUSIC,
    EXIT_PROGRAM,
} EVENT;

// -------------------------------------------------------------------------------------------------------

static const char *NAME_PROGRAM_FILE    = "PSW.asm";
static const char *NAME_FONT_FILE       = "TTF.font";
static const int SCREEN_WIDTH   = 400;
static const int SCREEN_HEIGHT  = 600;
static const int SIZE_POOL = 16;
static const SDL_Color FONT_COLOR = {255, 255, 255, 255};

// -------------------------------------------------------------------------------------------------------
int main() {
    size_t size = get_file_size(NAME_PROGRAM_FILE);
    char *buffer = create_char_buffer(size);

    FILE *stream = fopen(NAME_PROGRAM_FILE, "rb");
    fread(buffer, size, sizeof(char), stream);
    fclose(stream);


}



// ---------------------------------------------------------------------------------------------------



// ---------------------------------------------------------------------------------------------------
static create_display(display *disp) {
    assert(disp);

    SDL_Init(SDL_INIT_AUDIO);
    TTF_Init();

    SDL_Window *w = SDL_CreateWindow("Patcher", SDL_WINDOWPOS_CENTERED,
        SDL_WINDOWPOS_CENTERED, SCREEN_WIDTH, SCREEN_HEIGHT, 0);

    SDL_Renderer *r = SDL_CreateRenderer(w, -1,
        SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);

    TTF_Font *f = TTF_OpenFont(NAME_FONT_FILE, 24);

    disp->font = f;
    disp->ren = r;
    disp->win = w;


}

// ---------------------------------------------------------------------------------------------------
static void fill_display_pool(display *disp) {
    assert(disp);


}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Create a button object
 *
 * @param disp Pointer to display struct
 * @param text Text, which shows in button
 * @param place Position and size button
 * @param pr Color of pressed button
 * @param un Color of unpressed button
 * @return button_obj*
 */
static button_obj * create_button_object(display *disp, const char *text,
        SDL_Rect place, const SDL_Color pr, const SDL_Color un) {
    assert(disp);
    assert(text);

    /* Allocate memory for button struct */
    button_obj *but = (button_obj *)calloc(1, sizeof(button_obj));
    if (but == NULL) EXIT_FUNC("NULL Calloc", NULL);
    but->place = place;

    /* Create texture for text */
    SDL_Surface *sur_tex = TTF_RenderUTF8_Blended(disp->font, text, FONT_COLOR);
    SDL_Texture *text_tex = SDL_CreateTextureFromSurface(disp->ren, sur_tex);
    SDL_SetTextureBlendMode (text_tex, SDL_BLENDMODE_BLEND);
    SDL_FreeSurface (sur_tex);

    /* Find sizes of text texture and create rect for it */
    int w_text = 0;
    int h_text = 0;
    TTF_SizeUTF8(disp->font, text, &w_text, &h_text);

    SDL_Rect text_rect = { (place.w - w_text) / 2,
        (place.h - h_text) / 2, w_text, h_text };


    SDL_Rect but_rect = {0, 0, place.w, place.h};

    /* Create texture for combine text texture and background color */
    /* For press color */
    SDL_Texture *comb = SDL_CreateTexture(disp->ren, SDL_PIXELFORMAT_RGBA8888,
        SDL_TEXTUREACCESS_TARGET, place.w, place.h);

    SDL_SetTextureBlendMode(comb, SDL_BLENDMODE_BLEND);
    SDL_SetRenderTarget(disp->ren, comb);

    SDL_SetRenderDrawColor(disp->ren, pr.r, pr.g, pr.b, pr.a);
    SDL_RenderFillRect(disp->ren, &but_rect);

    SDL_RenderCopy(disp->ren, text_tex, NULL, &text_rect);
    but->pr = comb;

    /* For unpress color */
    comb = SDL_CreateTexture(disp->ren, SDL_PIXELFORMAT_RGBA8888,
        SDL_TEXTUREACCESS_TARGET, place.w, place.h);

    SDL_SetTextureBlendMode(comb, SDL_BLENDMODE_BLEND);
    SDL_SetRenderTarget(disp->ren, comb);

    SDL_SetRenderDrawColor(disp->ren, un.r, un.g, un.b, un.a);
    SDL_RenderFillRect(disp->ren, &but_rect);

    SDL_RenderCopy(disp->ren, text_tex, NULL, &text_rect);
    but->un = comb;

    /* Free memory */
    SDL_DestroyTexture(text_tex);

    SDL_SetRenderDrawColor(disp->ren, 0, 0, 0, 0);
    // SDL_RenderClear(disp->ren);
    SDL_SetRenderTarget(disp->ren, NULL);

    return but;
}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Create a text object
 *
 * @param disp Pointer ot display struct
 * @param text Showed text
 * @param place Position and size of window with text
 * @param col Color of background
 * @return text_obj*
 */
static text_obj * create_text_object(display *disp, const char *text,
        SDL_Rect place, const SDL_Color col) {
    assert(disp);
    assert(text);

    /* Allocate memory for text struct */
    text_obj *txt = (text_obj *)calloc(1, sizeof(text_obj));
    if (txt == NULL) EXIT_FUNC("NULL Calloc", NULL);
    txt->place = place;

    /* Create texture for text */
    SDL_Surface *sur_tex = TTF_RenderUTF8_Blended(disp->font, text, FONT_COLOR);
    SDL_Texture *text_tex = SDL_CreateTextureFromSurface(disp->ren, sur_tex);
    SDL_SetTextureBlendMode (text_tex, SDL_BLENDMODE_BLEND);
    SDL_FreeSurface (sur_tex);

    /* Find sizes of text texture and create rect for it */
    int w_text = 0;
    int h_text = 0;
    TTF_SizeUTF8(disp->font, text, &w_text, &h_text);

    SDL_Rect text_rect = { (place.w - w_text) / 2,
        (place.h - h_text) / 2, w_text, h_text };


    SDL_Rect txt_rect = {0, 0, place.w, place.h};

    /* Create texture for combine text texture and background color */
    SDL_Texture *comb = SDL_CreateTexture(disp->ren, SDL_PIXELFORMAT_RGBA8888,
        SDL_TEXTUREACCESS_TARGET, place.w, place.h);

    SDL_SetTextureBlendMode(comb, SDL_BLENDMODE_BLEND);
    SDL_SetRenderTarget(disp->ren, comb);

    SDL_SetRenderDrawColor(disp->ren, col.r, col.g, col.b, col.a);
    SDL_RenderFillRect(disp->ren, &txt_rect);

    SDL_RenderCopy(disp->ren, text_tex, NULL, &text_rect);
    txt->tex = comb;

    /* Free memory */
    SDL_DestroyTexture(text_tex);

    SDL_SetRenderDrawColor(disp->ren, 0, 0, 0, 0);
    // SDL_RenderClear(disp->ren);
    SDL_SetRenderTarget(disp->ren, NULL);

    return txt;
}


// ---------------------------------------------------------------------------------------------------
/**
 @brief Функция загрузки содержимого файла в буфер
 @param [in] name_file Имя загружаемого файла
 @return Указатель на выделенную динамически память
*/
static char * create_char_buffer(size_t size) {
    char *buffer = (char *)calloc(sizeof(char), size);
    if (buffer == NULL) EXIT_FUNC("NULL Calloc", NULL);

    return buffer;
}

// -------------------------------------------------------------------------------------------------------
/**
 * @brief Функция, находящая размер файла в байтах по его имени
 * @param [in] name_file Имя файла
 * @return Размер файла
*/
static size_t get_file_size(const char* name_file) {
    assert(name_file);

    struct stat file_s = {};

    if (stat(name_file, &file_s) == -1) EXIT_FUNC("Bad stat", 0);

    return (size_t) file_s.st_size;
}
