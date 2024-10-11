#include "command.h"

#include <stdio.h>

void
 mopr_print_command_queue( CommandQueue const * const commandQueue )
{
    printf( "COMMAND QUEUE INFO:\n" );
    printf( "commandQueue->nofCommands = %d\n", commandQueue->nofCommands );
    for ( int i = 0; i < commandQueue->nofCommands; ++i )
    {
        printf( "Contents of commandQueue->commands[ %d ]:\n", i );
        switch ( commandQueue->commands[ i ].base.cType )
        {
            case COMMAND_TYPE_BASE:
                printf( "  Type: BASE\n" );
                break;
            case COMMAND_TYPE_DRAW_ROOT_CONTAINER:
                printf( "  Type: DRAW_ROOT_CONTAINER\n" );
                break;
            case COMMAND_TYPE_DRAW_EXPR_CONTAINER:
                printf( "  Type: DRAW_EXPR_CONTAINER\n" );
                break;
            case COMMAND_TYPE_DRAW_EXPR_LABEL:
                printf( "  Type: DRAW_EXPR_LABEL\n" );
                break;
            case COMMAND_TYPE_DRAW_ATTR_LABEL:
                printf( "  Type: DRAW_ATTR_LABEL\n" );
                break;
            case COMMAND_TYPE_DRAW_ATTR_INPUT:
                printf( "  Type: DRAW_ATTR_INPUT\n" );
                break;
        }
    }
}
