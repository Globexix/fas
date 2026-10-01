#include <SDL2/SDL.h>
#include <stdio.h>

int main(void) {
  if (SDL_Init(SDL_INIT_EVENTS) != 0) return 1;
  SDL_Event event;
  SDL_zero(event);
  event.type = SDL_USEREVENT;
  event.user.code = 7;
  int pushed = SDL_PushEvent(&event);
  SDL_Event received;
  SDL_zero(received);
  int polled = SDL_PollEvent(&received);
  printf("%llu %d %d\n", (unsigned long long)sizeof(SDL_Event), pushed,
         received.user.code);
  SDL_Quit();
  return pushed == 1 && polled == 1 && received.user.code == 7 ? 0 : 2;
}
