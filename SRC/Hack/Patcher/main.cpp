#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_mixer.h>

#include "func.h"
#include "hash_func.h"
#include "const.h"
#include "type.h"

// Exit functions with auto print error
// --------------------------------------------------------------------------------------------------
#define EXIT_FUNC(reason, return_value) \
 do { printf ("ERROR in \"%s:%d\": %s\n", __FILE__, __LINE__, reason); \
 return return_value; } while (0)
// --------------------------------------------------------------------------------------------------


// -------------------------------------------------------------------------------------------------------
int main() {
//     size_t size = get_file_size(NAME_PROGRAM_FILE);
//     char *buffer = create_char_buffer(size);
//
//     FILE *stream = fopen(NAME_PROGRAM_FILE, "rb");
//     fread(buffer, size, sizeof(char), stream);
//     fclose(stream);

    patcher_UI();

    return 0;
}

// ---------------------------------------------------------------------------------------------------
void patcher_UI(void) {

    display disp = {0};
    create_display(&disp);
    int cur_col = 0;
    bool is_patched = false;
    bool is_music_played = true;

    // printf("Create_Display \n");
//
//     while (1) {
//         renew_display(&disp, cur_col);
//         // return ;
//     }

    EVENT event = NULL_EVENT;
    while ((event = get_event(&disp, cur_col)) != EXIT_PROGRAM) {
        switch (event) {
            case START_PROGRAM:
                cur_col = _MENU_NOT_PATCHED;
                renew_display(&disp, cur_col);
                break;

            case PATCH_FILE:
                if (is_patched == true) cur_col = _MENU_ALREADY_PATCHED;
                else {
                    cur_col = patch_file(NAME_PROGRAM_FILE);
                    if (cur_col != _MENU_UNKNOWN_FILE)  is_patched = true;
                }
                break;

            case CONTROL_MUSIC:
                if (is_music_played) {
                    Mix_PauseMusic();
                    is_music_played = false;
                } else {
                    Mix_ResumeMusic ();
                    is_music_played = true;
                }
                break;

            case EXIT_PROGRAM:
            case NULL_EVENT:
            default:
                destroy_display(&disp);
                return ;
        }
    }
    return ;
}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Patch file with name `name`
 *
 * @param name Name of file
 * @return int  New collection
 */
int patch_file(const char *name) {
    assert(name);

    size_t size_file = get_file_size(name);
    if (size_file == 0) return _MENU_UNKNOWN_FILE;

    char *buffer = create_file_buffer(name);

    int cor_value = check_correct_file(buffer, size_file);
    if (cor_value == -1)    return _MENU_UNKNOWN_FILE;
    if (cor_value == 1)     return _MENU_ALREADY_PATCHED;

    patch_program(buffer);

    return _MENU_PATCHED;
}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Check, is file correct
 *
 * @param buffer Buffer with data from file
 * @param size Size buffer
 * @return int 0 - if file not patched; 1 - if file patched; -1 - if file not cirrect
 */
int check_correct_file(char *buffer, size_t size) {
    assert(buffer);

    hash_t hash_file = get_hash(buffer, (int) size);
    if (hash_equal(hash_file, NOT_PATCHED_FILE_HASH) == 1) return 0;
    else if (hash_equal(hash_file, PATCHED_FILE_HASH) == 1) return 1;
    return -1;
}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Patch file with its buffer
 *
 * @param buffer Data from file
 */
static void patch_program(char *buffer) {
    assert(buffer);

    /* Crack first weak */
    buffer[0xD6] = (char) 0;
    buffer[0xD7] = (char) 0;


    /* Crack second weak */
    buffer[0x1C] = (char) 0xEB;
    buffer[0x1D] = (char) 0x18;

    return ;
}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Get the event object
 *
 * @param disp Struct of data for display
 * @param cur_col Currect menu
 * @return EVENT
 */
static EVENT get_event(display *disp, int cur_col) {
    assert(disp);

    // printf("Give event\n");
    OBJECT_ID *array = disp->menu.collect[cur_col].pool;
    object_entry *pool = disp->pool;
    int size = disp->menu.collect[cur_col].size;

    bool running = true;
    SDL_Event event = {0};

    while (running) {
        while (SDL_PollEvent(&event)) {

            if (event.type == SDL_QUIT) {
                return EXIT_PROGRAM;
            }

            if (event.type == SDL_MOUSEMOTION) {
                int mouse_x = event.motion.x;
                int mouse_y = event.motion.y;
                for (int i = 0; i < size; i++) {
                    if (pool[array[i]].kind != BUTTON_OBJECT) continue;

                    check_button_hover(pool[array[i]].data.but, mouse_x, mouse_y);
                }
                // renew_display(disp, cur_col);
            }

            int number_but = -1;
            for (int i = 0; i < size; i++) {
                if (pool[array[i]].kind != BUTTON_OBJECT) continue;

                if (check_button_click(pool[array[i]].data.but,
                    &event) == false) continue;

                number_but = i;
                break;
            }

            renew_display(disp, cur_col);

            if (number_but == -1) continue;

            if (cur_col == _MENU_INIT) {
                switch (number_but) {
                    case _BUTTON_AGREE: return START_PROGRAM;
                    case _BUTTON_NOT_AGREE: return EXIT_PROGRAM;
                    default: return EXIT_PROGRAM;
                }
            } else {
                switch (number_but) {
                    case _BUTTON_EXIT: return EXIT_PROGRAM;
                    case _BUTTON_PATCH: return PATCH_FILE;
                    case _BUTTON_MUSIC: return CONTROL_MUSIC;
                    default: return EXIT_PROGRAM;
                }
            }
            return EXIT_PROGRAM;
        }
        renew_display(disp, cur_col);
    }
    return EXIT_PROGRAM;
}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Create a display object
 *
 * @param disp Pointer to allocate memory for display struct
 */
static void create_display(display *disp) {
    assert(disp);


    SDL_Init(SDL_INIT_AUDIO);
    TTF_Init();

    Mix_OpenAudio (44100, MIX_DEFAULT_FORMAT, 2, 2048);
    Mix_Init(MIX_INIT_MP3);

    SDL_Window *w = SDL_CreateWindow("Patcher", SDL_WINDOWPOS_CENTERED,
        SDL_WINDOWPOS_CENTERED, SCREEN_WIDTH, SCREEN_HEIGHT, 0);

    SDL_Renderer *r = SDL_CreateRenderer(w, -1,
        SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);

    TTF_Font *f = TTF_OpenFont(NAME_FONT_FILE, 24);

    disp->font = f;
    disp->ren = r;
    disp->win = w;

    disp->music = Mix_LoadMUS(FONE_MUSIC);
    if (disp->music == NULL) EXIT_FUNC("NULL Fone Music", );

    Mix_PlayMusic(disp->music, -1);
    Mix_VolumeMusic(VOLUME_FONE_MUSIC);

    SDL_SetRenderDrawColor(r, 30, 30, 30, 255);
    SDL_RenderClear(r);

    // SDL_RenderPresent(r);
    // printf("%p\n", w);

    // while (1) {
    //     int a = 0;
    // }

    fill_display_pool(disp);
    renew_display(disp, 0);

    return ;
}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Destroy display struct
 *
 * @param disp Pointer to display struct
 */
static void destroy_display(display *disp) {
    assert(disp);

    for (int i = 0; i < disp->size_pool; i++) {
        object_entry entry = disp->pool[i];
        if (entry.kind == BUTTON_OBJECT) {
            SDL_DestroyTexture(entry.data.but->pr);
            SDL_DestroyTexture(entry.data.but->un);
            free(entry.data.but);
        }
        else {
            SDL_DestroyTexture(entry.data.text->tex);
            free(entry.data.text);
        }
    }
    TTF_CloseFont(disp->font);
    TTF_Quit();

    SDL_DestroyRenderer(disp->ren);
    SDL_DestroyWindow(disp->win);
    SDL_Quit();

    Mix_FreeMusic(disp->music);
    Mix_CloseAudio();
    Mix_Quit();

    return ;
}


// ---------------------------------------------------------------------------------------------------
/**
 * @brief Fill pool in display struct
 *
 * @param disp Pointer to struct of data for display
 */
static void fill_display_pool(display *disp) {
    assert(disp);

    int index = 0;
    for (int i = 0; i < AMOUNT_BUTTONS; i++) {
        button_obj *but = create_button_object(disp, BUTTON_TEXT[i],
            BUTTON_PLACES[i], BUTTON_PR_COLOR, BUTTON_UN_COLOR);
        disp->pool[index].data.but = but;
        disp->pool[index].kind = BUTTON_OBJECT;
        index++;
    }

    text_obj *text_window = create_text_object(disp, TEXT_TEXT[0],
        TEXT_PLACE[0], TEXT_COLOR);
    disp->pool[index].data.text = text_window;
    disp->pool[index].kind = TEXT_OBJECT;
    index++;

    for (int i = 1; i < 4; i++) {
        text_window = create_text_object(disp, TEXT_TEXT[i],
            TEXT_PLACE[1], TEXT_COLOR);
        disp->pool[index].data.text = text_window;
        disp->pool[index].kind = TEXT_OBJECT;
        index++;
    }

    disp->size_pool = index;

    object_collection *col_1 = &(disp->menu.collect[_MENU_INIT]);
    col_1->pool[_BUTTON_AGREE] = 0;
    col_1->pool[_BUTTON_NOT_AGREE] = 1;
    col_1->pool[_TEXT_POLICY] = 5;
    col_1->size = 3;

    col_1 = &(disp->menu.collect[_MENU_NOT_PATCHED]);
    col_1->pool[_BUTTON_EXIT] = 2;
    col_1->pool[_BUTTON_PATCH] = 3;
    col_1->pool[_BUTTON_MUSIC] = 4;
    col_1->size = 3;

    col_1 = &(disp->menu.collect[_MENU_PATCHED]);
    col_1->pool[_BUTTON_EXIT] = 2;
    col_1->pool[_BUTTON_PATCH] = 3;
    col_1->pool[_BUTTON_MUSIC] = 4;
    col_1->pool[_TEXT_SUCCESS] = 6;
    col_1->size = 4;

   col_1 = &(disp->menu.collect[_MENU_ALREADY_PATCHED]);
    col_1->pool[_BUTTON_EXIT] = 2;
    col_1->pool[_BUTTON_PATCH] = 3;
    col_1->pool[_BUTTON_MUSIC] = 4;
    col_1->pool[_TEXT_SUCCESS] = 7;
    col_1->size = 4;

    col_1 = &(disp->menu.collect[_MENU_UNKNOWN_FILE]);
    col_1->pool[_BUTTON_EXIT] = 2;
    col_1->pool[_BUTTON_PATCH] = 3;
    col_1->pool[_BUTTON_MUSIC] = 4;
    col_1->pool[_TEXT_SUCCESS] = 8;
    col_1->size = 4;

    disp->menu.amount = 5;
    return ;
}

// ---------------------------------------------------------------------------------------------------
static void renew_display(display *disp, int cur_col) {
    assert(disp);

    OBJECT_ID *array = disp->menu.collect[cur_col].pool;
    object_entry *pool = disp->pool;
    int size = disp->menu.collect[cur_col].size;

    SDL_SetRenderDrawColor(disp->ren, 30, 30, 30, 255);
    SDL_RenderClear(disp->ren);


    for (int i = 0; i < size; i++) {
        object_entry entry = pool[array[i]];

        if (entry.kind == BUTTON_OBJECT) {
            if (entry.data.but->is_press || entry.data.but->is_hover) {
                // printf("%p\n", entry.data.but->pr);
                SDL_RenderCopy(disp->ren, entry.data.but->pr, NULL,
                    &(entry.data.but->place));
            } else {

                SDL_RenderCopy(disp->ren, entry.data.but->un, NULL,
                    &(entry.data.but->place));
            }
        } else {
            SDL_RenderCopy(disp->ren, entry.data.text->tex, NULL,
                &(entry.data.text->place));
        }
    }

    SDL_RenderPresent(disp->ren);
    return ;
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
    but->is_press = false;
    but->is_hover = false;

    /* Create texture for text */
    SDL_Surface *sur_tex = TTF_RenderUTF8_Blended(disp->font, text, FONT_COLOR);
    // printf("%p\n", sur_tex);
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
 * @brief Check button click
 *
 * @param but Struct of button object
 * @param ev Current event of SDL
 * @return true If button had clicked
 * @return false If button hadn't clicked
 */
static bool check_button_click(button_obj *but, SDL_Event *ev) {
    assert(but);
    assert(ev);

    if (ev->type == SDL_MOUSEBUTTONDOWN &&
        ev->button.button == SDL_BUTTON_LEFT) {
        int mouse_x = ev->button.x;
        int mouse_y = ev->button.y;

        if (mouse_x >= but->place.x &&
            mouse_x <= but->place.x + but->place.w &&
            mouse_y >= but->place.y &&
            mouse_y <= but->place.y + but->place.h) {
            but->is_press = true;
            return true;
        }
    }
    but->is_press = false;
    return false;
}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Check button hover
 *
 * @param but Struct of button object
 * @param x Coordinate mouse on x
 * @param y Coordinate mouse on y
 * @return true If button had clicked
 * @return false If button hadn't clicked
 */
static bool check_button_hover(button_obj *but, int x, int y) {
    assert(but);

    if (x >= but->place.x &&
        x <= but->place.x + but->place.w &&
        y >= but->place.y &&
        y <= but->place.y + but->place.h) {
        but->is_hover = true;
        return true;
    }

    but->is_hover = false;
    return false;
}


// ---------------------------------------------------------------------------------------------------
/**
 * @brief Create a text object
 *
 * @param disp Pointer to display struct
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

    /* Create texture for multi-line text */
    SDL_Surface *sur_tex = render_multiline_text(disp->font, text);
    if (sur_tex == NULL) { free(txt); EXIT_FUNC("render_multiline_text failed", NULL); }

    SDL_Texture *text_tex = SDL_CreateTextureFromSurface(disp->ren, sur_tex);
    SDL_FreeSurface(sur_tex);
    if (text_tex == NULL) { free(txt); EXIT_FUNC("CreateTexture failed", NULL); }

    SDL_SetTextureBlendMode(text_tex, SDL_BLENDMODE_BLEND);

    /* Get texture dimensions */
    int w_text = 0;
    int h_text = 0;
    SDL_QueryTexture(text_tex, NULL, NULL, &w_text, &h_text);

    SDL_Rect text_rect = { (place.w - w_text) / 2,
        (place.h - h_text) / 2, w_text, h_text };

    SDL_Rect txt_rect = {0, 0, place.w, place.h};

    /* Create texture for combine text texture and background color */
    SDL_Texture *comb = SDL_CreateTexture(disp->ren, SDL_PIXELFORMAT_RGBA8888,
        SDL_TEXTUREACCESS_TARGET, place.w, place.h);
    if (comb == NULL) {
        SDL_DestroyTexture(text_tex);
        free(txt);
        EXIT_FUNC("CreateTexture comb failed", NULL);
    }

    SDL_SetTextureBlendMode(comb, SDL_BLENDMODE_BLEND);
    SDL_SetRenderTarget(disp->ren, comb);

    SDL_SetRenderDrawColor(disp->ren, col.r, col.g, col.b, col.a);
    SDL_RenderFillRect(disp->ren, &txt_rect);

    SDL_RenderCopy(disp->ren, text_tex, NULL, &text_rect);
    txt->tex = comb;

    /* Free memory */
    SDL_DestroyTexture(text_tex);

    SDL_SetRenderDrawColor(disp->ren, 0, 0, 0, 0);
    SDL_SetRenderTarget(disp->ren, NULL);

    return txt;
}

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Render multi-line text
 *
 * @param font Pointer to TTF_Font
 * @param text Text to render
 * @return SDL_Surface* - Combined surface with all lines
 */
static SDL_Surface * render_multiline_text(TTF_Font *font, const char *text) {
    assert(font);
    assert(text);

    /* Count lines */
    int line_count = 1;
    for (const char *p = text; *p; p++) {
        if (*p == '\n') line_count++;
    }

    /* Find max width and line height */
    int max_width = 0;
    int line_height = TTF_FontHeight(font);
    char *text_copy = strdup(text);
    char *saveptr = NULL;
    char *line = strtok_r(text_copy, "\n", &saveptr);

    while (line != NULL) {
        int w = 0, h = 0;
        TTF_SizeUTF8(font, line, &w, &h);
        if (w > max_width) max_width = w;
        line = strtok_r(NULL, "\n", &saveptr);
    }
    free(text_copy);

    /* Create final surface */
    int total_height = line_height * line_count;
    SDL_Surface *final_sur = SDL_CreateRGBSurfaceWithFormat(0,
        max_width, total_height, 32, SDL_PIXELFORMAT_RGBA8888);
    if (final_sur == NULL) return NULL;

    /* Fill with transparent background */
    SDL_FillRect(final_sur, NULL, SDL_MapRGBA(final_sur->format, 0, 0, 0, 0));

    /* Render each line */
    int y_offset = 0;
    text_copy = strdup(text);
    line = strtok_r(text_copy, "\n", &saveptr);

    while (line != NULL) {
        SDL_Surface *line_sur = TTF_RenderUTF8_Blended(font, line, FONT_COLOR);
        if (line_sur) {
            SDL_Rect dest = {0, y_offset, line_sur->w, line_sur->h};
            SDL_BlitSurface(line_sur, NULL, final_sur, &dest);
            SDL_FreeSurface(line_sur);
            y_offset += line_height;
        }
        line = strtok_r(NULL, "\n", &saveptr);
    }
    free(text_copy);

    return final_sur;
}

// ---------------------------------------------------------------------------------------------------
/**
 @brief Функция загрузки содержимого файла в буфер
 @param [in] name_file Имя загружаемого файла
 @return Указатель на выделенную динамически память
*/
static char * create_char_buffer(size_t size) {
    char *buffer = (char *)calloc(size, sizeof(char));
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

// -------------------------------------------------------------------------------------------------------
/**
 * @brief Create a buffer with data from file
 *
 * @param name_file Name file with data
 * @return char*
 */
static char * create_file_buffer(const char *name_file) {
    assert(name_file);

    size_t size_file = get_file_size(name_file);
    char *buffer = create_char_buffer(size_file + 1);

    FILE *stream = fopen(name_file, "rb");
    if (stream == NULL) {
        free(buffer);
        EXIT_FUNC("NULL File", NULL);
    }

    size_t amount_chars = fread(buffer, sizeof(char), size_file, stream);
    fclose(stream);
    buffer[amount_chars] = '\0';

    return buffer;
}
