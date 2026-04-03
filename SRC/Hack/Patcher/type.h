#ifndef TYPE_H
#define TYPE_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_mixer.h>

// ---------------------------------------------------------------------------------------------------
#define SIZE_POOL 16
#define SIZE_HASH 32

// ---------------------------------------------------------------------------------------------------
typedef int OBJECT_ID;

// ---------------------------------------------------------------------------------------------------
typedef struct text_obj {
    SDL_Rect place;
    SDL_Texture *tex;
} text_obj;

typedef struct button_obj {
    SDL_Rect place;

    SDL_Texture *pr;
    SDL_Texture *un;

    bool is_press;
    bool is_hover;
} button_obj;

// ---------------------------------------------------------------------------------------------------
typedef enum object_kind {
    TEXT_OBJECT,
    BUTTON_OBJECT,
} object_kind;

typedef struct object_entry {
    object_kind kind;

    union {
        text_obj *text;
        button_obj *but;
    } data;
} object_entry;

typedef struct object_collection {
    int size;
    OBJECT_ID pool[10];
} object_collection;

// ---------------------------------------------------------------------------------------------------
typedef struct display {
    SDL_Window *win;
    SDL_Renderer *ren;
    TTF_Font *font;
    Mix_Music *music;

    object_entry pool[SIZE_POOL];
    int size_pool;

    struct {
        object_collection collect[10];
        int amount;
    } menu;
} display;

// ---------------------------------------------------------------------------------------------------
typedef enum EVENT {
    NULL_EVENT,
    START_PROGRAM,
    PATCH_FILE,
    CONTROL_MUSIC,
    EXIT_PROGRAM,
} EVENT;

// ---------------------------------------------------------------------------------------------------
typedef size_t hash_t __attribute__((vector_size(SIZE_HASH)));


#endif /* TYPE_H */
