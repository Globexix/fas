#include <stdio.h>

struct WeaponInfo {
    int ammo;
    int upstate;
    int downstate;
    int readystate;
    int atkstate;
    int flashstate;
};

struct SfxInfo {
    const char *name;
    const struct SfxInfo *link;
};

int main(void) {
    const unsigned char rndtable[4] = {3, 5, 7, 11};
    const struct WeaponInfo weaponinfo[2] = {
        {0, 1, 2, 3, 4, 5},
        {1, 11, 12, 13, 17, 18}
    };
    const struct SfxInfo sfx[2] = {
        {"pistol", NULL},
        {"rapid", &sfx[0]}
    };
    int values[3] = {1 + 2, 4, 5};
    int result = (values[0] + values[1]) * 10 + rndtable[1 + 1];
    int checked = result;
    int linked = sfx[1].link == &sfx[0];
    printf("%d %d %d %d\n", checked, values[2], weaponinfo[1].atkstate, linked);
    return 0;
}
