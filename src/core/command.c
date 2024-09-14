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
            case COMMAND_TYPE_NONE:
                printf( "  Type: NONE\n" );
                break;
            case COMMAND_TYPE_DRAW_RECT:
                printf( "  Type: DRAW_RECT\n" );
                break;
            case COMMAND_TYPE_DRAW_CIRCLE:
                printf( "  Type: DRAW_CIRCLE\n" );
                break;
        }
    }
}
