#include "debugutils.h"
#include "bmp.h"

#include <ctime>
#include <cstdio>
#include <cstring>
#include <string>
#include <stdexcept>
#include <windows.h>

#include <GL/glew.h>
#include <GL/freeglut.h>

static GLuint texture;
static int width, height, padded_width;

uint8_t local_buf[1024*1024*3];

void gldraw(uint8_t buf[]) {
  DEBUG_TRACE("");
  std::memcpy(local_buf, buf, padded_width*height);

#if DEBUG_LEVEL == 0
  Sleep(10); // XXX TODO hack: use inter-thread communication next time
#endif
}

static void Redrew(int) {
  DEBUG_TRACE("");
  glutPostRedisplay();
  glutTimerFunc(30, Redrew, 0);
}

static void RenderSceneCB() {
  DEBUG_TRACE("");

  // draw the image to the texture
  glTexImage2D( GL_TEXTURE_2D
              , 0
              , GL_RGBA
              , width
              , height
              , 0
              , GL_BGR
              , GL_UNSIGNED_BYTE
              , local_buf );

  // draw the image on the screen
  glBegin(GL_QUADS);
  glTexCoord2f(0,1);glVertex2f(-1, 1);
  glTexCoord2f(0,0);glVertex2f(-1,-1);
  glTexCoord2f(1,0);glVertex2f( 1,-1);
  glTexCoord2f(1,1);glVertex2f( 1, 1);
  glEnd();

  glutSwapBuffers();
}

static HANDLE hThread;
static DWORD dwTID;

static DWORD WINAPI runGlutMain(LPVOID) {
  DEBUG_TRACE("");

  int argc = 1;
  char argv0[] = "mpeg";
  char *argv[] = {argv0};

  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);
  glutInitWindowSize(width, height);
  glutInitWindowPosition(10, 10);
  glutCreateWindow("MPEG-1");

  GLenum res = glewInit();
  if (res != GLEW_OK) {
    std::string msg("glewInit() error: ");
    msg += reinterpret_cast<const char*>(glewGetErrorString(res));
    throw std::runtime_error(msg.c_str());
  }

  // initialize texture
  glGenTextures(1, &texture);
  glBindTexture(GL_TEXTURE_2D, texture);

  glEnable(GL_TEXTURE_2D);

  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // setup callback function
  glutDisplayFunc(RenderSceneCB);
  glutTimerFunc(30, Redrew, 0);

  // enter event main loop
  glutMainLoop();
  // no return
  return 0;
}

void glrun(int height_, int width_) {
  DEBUG_TRACE("");

  width = width_;
  height = height_;
  padded_width = (width*3+3)/4*4;

  hThread = CreateThread(NULL, 0, runGlutMain, NULL, 0, &dwTID);
  if (hThread == NULL) {
    DWORD err = GetLastError();

    char* msgbuf;
    FormatMessage(
      FORMAT_MESSAGE_ALLOCATE_BUFFER|FORMAT_MESSAGE_FROM_SYSTEM,
      NULL,
      err,
      0,
      (LPSTR)&msgbuf,
      0,
      NULL
    );
    std::string msg("glrun(): CreateThread error ");
    msg += std::to_string(err);
    msg += " (";
    msg += msgbuf;
    msg += ")";

    LocalFree(msgbuf);

    throw std::runtime_error(msg.c_str());
  }
}
