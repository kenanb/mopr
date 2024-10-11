#ifndef MOPR_CORE_COMMAND_H
#define MOPR_CORE_COMMAND_H

// Generic includes.
#include "base/api.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum CommandTheme
    {
        COMMAND_THEME_NONE = 0,
        COMMAND_THEME_ROOT_CONTAINER_FG,
        COMMAND_THEME_ROOT_CONTAINER_BG,
        COMMAND_THEME_EXPR_CONTAINER_FG,
        COMMAND_THEME_EXPR_CONTAINER_BG,
        COMMAND_THEME_ATTR_INPUT_FG,
        COMMAND_THEME_ATTR_INPUT_BG,
        COMMAND_THEME_ATTR_LABEL_FG,
        COMMAND_THEME_EXPR_LABEL_FG,
        COMMAND_THEME_EXPR_BG_0,
        COMMAND_THEME_EXPR_BG_1,
        COMMAND_THEME_EXPR_BG_2,
        COMMAND_THEME_EXPR_BG_3,
        COMMAND_THEME_EXPR_BG_4,
        COMMAND_THEME_EXPR_BG_5,
        COMMAND_THEME_EXPR_BG_6,
        COMMAND_THEME_EXPR_BG_7,
        COMMAND_THEME_EXPR_BG_8,
        COMMAND_THEME_EXPR_BG_9,
        COMMAND_THEME_TERMINATOR
    } CommandTheme;

    typedef enum CommandType
    {
        COMMAND_TYPE_BASE = 0,
        COMMAND_TYPE_DRAW_ROOT_CONTAINER,
        COMMAND_TYPE_DRAW_EXPR_CONTAINER,
        COMMAND_TYPE_DRAW_EXPR_LABEL,
        COMMAND_TYPE_DRAW_ATTR_LABEL,
        COMMAND_TYPE_DRAW_ATTR_INPUT,
    } CommandType;

#define COMMAND_BASE_MEMBERS                                                             \
    CommandType cType;                                                                   \
    float x, y, w, h;

    typedef struct CommandBase
    {
        COMMAND_BASE_MEMBERS
    } CommandBase;

    typedef struct CommandDrawRootContainer
    {
        COMMAND_BASE_MEMBERS
    } CommandDrawRootContainer;

    typedef struct CommandDrawExprContainer
    {
        COMMAND_BASE_MEMBERS
    } CommandDrawExprContainer;

    typedef struct CommandDrawExprLabel
    {
        COMMAND_BASE_MEMBERS
        CommandTheme bg;
        char * text;
    } CommandDrawExprLabel;

    typedef struct CommandDrawAttrLabel
    {
        COMMAND_BASE_MEMBERS
        CommandTheme bg;
        char * text;
    } CommandDrawAttrLabel;

    typedef struct CommandDrawAttrInput
    {
        COMMAND_BASE_MEMBERS
        char * text;
    } CommandDrawAttrInput;

    typedef union CombinedCommand
    {
        CommandBase base;
        CommandDrawRootContainer drawRootContainer;
        CommandDrawExprContainer drawExprContainer;
        CommandDrawExprLabel drawExprLabel;
        CommandDrawAttrLabel drawAttrLabel;
        CommandDrawAttrInput drawAttrInput;
    } CombinedCommand;

    typedef struct CommandQueue
    {
        float pixelsW, pixelsH;
        int nofCommands;
        union CombinedCommand * commands;
    } CommandQueue;

    MOPR_API void
     mopr_print_command_queue( CommandQueue const * const commandQueue );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_CORE_COMMAND_H
