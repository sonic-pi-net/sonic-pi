#include <stdio.h>
#include "sp_link.h"

int main()
{
    printf("Starting link integration\n");
    sp_link_init(120);
    printf("Waiting for keypress and then enabling link\n");
    getchar();
    sp_link_enable(1);
    printf("Waiting for keypress and then getting number of peers\n");
    getchar();
    int num_peers;
    sp_link_get_num_peers(&num_peers);
    printf("Number of peers: %d\n", num_peers);
    printf("Waiting for keypress and then changing tempo to 88\n");
    getchar();
    sp_link_set_tempo(88, 0);
    printf("Waiting for keypress and then showing current tempo\n");
    getchar();
    double bpm;
    sp_link_get_tempo(&bpm);
    printf("Tempo: %lf\n", bpm);
    printf("Waiting for keypress and then disabling link\n");
    getchar();
    sp_link_enable(0);
    printf("Waiting for keypress and then exiting\n");
    sp_link_deinit();

    return 0;
}
