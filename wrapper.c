#include "raylib.h"
#include <stdlib.h>

// Wrapper to handle pointer from droppedFiles
char* filePathWrapper(int i, FilePathList droppedFiles) {
  return droppedFiles.paths[i];
}

