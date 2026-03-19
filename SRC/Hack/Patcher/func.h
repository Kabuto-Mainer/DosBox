#ifndef FUNC_H
#define FUNC_H

#include "type.h"

void patcher_UI(void);
// void patch_file(char *buffer);

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Get the event object
 *
 * @param disp Struct of data for display
 * @param cur_col Currect menu
 * @return EVENT
 */
static EVENT get_event(display *disp, int cur_col);

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Create a display object
 *
 * @param disp Pointer to allocate memory for display struct
 */
static void create_display(display *disp);

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Destroy display struct
 *
 * @param disp Pointer to display struct
 */
static void destroy_display(display *disp);

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Fill pool in display struct
 *
 * @param disp Pointer to struct of data for display
 */
static void fill_display_pool(display *disp);

// ---------------------------------------------------------------------------------------------------
static void renew_display(display *disp, int cur_col);

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
        SDL_Rect place, const SDL_Color pr, const SDL_Color un);

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Check button click
 *
 * @param but Struct of button object
 * @param ev Current event of SDL
 * @return true If button had clicked
 * @return false If button hadn't clicked
 */
static bool check_button_click(button_obj *but, SDL_Event *ev);

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
static bool check_button_hover(button_obj *but, int x, int y);

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
        SDL_Rect place, const SDL_Color col);

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Render multi-line text
 *
 * @param font Pointer to TTF_Font
 * @param text Text to render
 * @return SDL_Surface* - Combined surface with all lines
 */
static SDL_Surface * render_multiline_text(TTF_Font *font, const char *text);

// ---------------------------------------------------------------------------------------------------
/**
 @brief Функция загрузки содержимого файла в буфер
 @param [in] name_file Имя загружаемого файла
 @return Указатель на выделенную динамически память
*/
static char * create_char_buffer(size_t size);

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Функция, находящая размер файла в байтах по его имени
 * @param [in] name_file Имя файла
 * @return Размер файла
*/
static size_t get_file_size(const char* name_file);

// -------------------------------------------------------------------------------------------------------
/**
 * @brief Create a buffer with data from file
 *
 * @param name_file Name file with data
 * @return char*
 */
static char * create_file_buffer(const char *name_file);


// =======================================================================================

// =======================================================================================

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Patch file with name `name`
 *
 * @param name Name of file
 * @return int  New collection
 */
int patch_file(const char *name);

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Check, is file correct
 *
 * @param buffer Buffer with data from file
 * @param size Size buffer
 * @return int 0 - if file not patched; 1 - if file patched; -1 - if file not cirrect
 */
int check_correct_file(char *buffer, size_t size);

// ---------------------------------------------------------------------------------------------------
/**
 * @brief Patch file with its buffer
 *
 * @param buffer Data from file
 */
static void patch_program(char *buffer);





#endif /* FUNC_H */
