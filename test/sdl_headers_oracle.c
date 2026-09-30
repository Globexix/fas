#include <SDL2/SDL.h>
#include <stdint.h>
#include <stdio.h>

static uint64_t render_checksum(uint32_t *pixels, int pitch) {
    int stride = pitch / 4;
    for (int frame = 0; frame < 24; frame++) {
        for (int x = 0; x < 320; x++) {
            int intensity = (x * 17 + frame * 31 + 43) & 255;
            pixels[199 * stride + x] = (uint32_t)intensity * 65793u;
        }
        for (int y = 198; y >= 0; y--) {
            for (int x = 0; x < 320; x++) {
                int left_x = x - 1;
                if (left_x < 0) left_x = 0;
                int right_x = x + 1;
                if (right_x > 319) right_x = 319;
                int row = (y + 1) * stride;
                int left = (int)(pixels[row + left_x] & 255u);
                int center = (int)(pixels[row + x] & 255u);
                int right = (int)(pixels[row + right_x] & 255u);
                int heat = (left + center + right) / 3;
                int cooling = (x + y + frame) & 3;
                int value = heat - cooling;
                if (value < 0) value = 0;
                pixels[y * stride + x] = (uint32_t)value * 65793u;
            }
        }
    }
    uint64_t hash = UINT64_C(14695981039346656037);
    const unsigned char *bytes = (const unsigned char *)pixels;
    for (int y = 0; y < 200; y++) {
        for (int byte = 0; byte < 320 * 4; byte++) {
            hash = (hash ^ bytes[y * pitch + byte]) * UINT64_C(1099511628211);
        }
    }
    return hash;
}

int main(void) {
    if (SDL_Init(SDL_INIT_VIDEO) != 0) return 1;
    SDL_Window *window = SDL_CreateWindow("c-sdl-fire", SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED, 320, 200, 0);
    if (!window) { SDL_Quit(); return 2; }
    SDL_Surface *surface = SDL_CreateRGBSurface(0, 320, 200, 32, 0, 0, 0, 0);
    if (!surface) { SDL_DestroyWindow(window); SDL_Quit(); return 3; }
    uint64_t hash = render_checksum((uint32_t *)surface->pixels, surface->pitch);
    int pushed = 0;
    SDL_Event event;
    SDL_zero(event);
    event.type = SDL_USEREVENT;
    while (pushed < 19) {
        if (SDL_PushEvent(&event) != 1) return 4;
        pushed++;
    }
    int events = 0;
    while (SDL_PollEvent(&event)) if (event.type == SDL_USEREVENT) events++;
    printf("%llu %d\n", (unsigned long long)hash, events);
    SDL_FreeSurface(surface);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 0;
}
