#ifndef MOPR_CORE_COMMAND_H
#define MOPR_CORE_COMMAND_H

// Generic includes.
#include "base/api.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum CommandType
    {
        COMMAND_TYPE_NONE = 0,
        COMMAND_TYPE_DRAW_RECT = 1,
        COMMAND_TYPE_DRAW_CIRCLE = 2,
    } CommandType;

    typedef struct CommandNone
    {
        CommandType cType;
    } CommandNone;

    typedef struct CommandDrawRect
    {
        CommandType cType;
        float x, y, w, h;
        unsigned char col[ 4 ];
        float rounding;
        char * text;
    } CommandDrawRect;

    typedef struct CommandDrawCircle
    {
        CommandType cType;
        float x, y, w, h;
        unsigned char col[ 4 ];
        float rounding;
        char * text;
    } CommandDrawCircle;

    typedef union CombinedCommand
    {
        CommandNone base;
        CommandDrawRect drawRect;
        CommandDrawCircle drawCircle;
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
