#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

double* transpose(double* mat, int r, int c) {
    double *p1 =(double *) malloc(r * c * sizeof(double));
    for (int i = 0; i < r; i++) {
        for (int j = 0; j < c; j++) {
            p1[j * r + i] = mat[i * c + j];
        }
    }
    for (int i = 0; i < r; i++) {
        for (int j = 0; j < c; j++) {
            mat[i * c + j] = p1[i * c + j];
        }
    }
    free(p1);
    return mat;
}

void scale_c(double* mat, int r, int c,  double ratio) {
    for (int i = 0; i < r; i++) {
        for (int j = 0; j < c; j++) {
            mat[i * c + j] *= ratio;
        }
    }

}

double* rorate(double* mat, int r, int c, bool dir) {
    mat = transpose(mat, r, c);
    double *p1 = (double *)malloc(r * c * sizeof(double));
    if (dir == true) {//left
        for (int i = 0; i < r; i++) {
            for (int j = 0; j < c; j++) {
                p1[(r - 1 - i) * c + j] = mat[i * c + j];
            }
        }
    }
    else {
        for (int i = 0; i < r; i++) {
            for (int j = 0; j < c; j++) {
                p1[i * c + (c - 1 - j)] = mat[i * c + j];
            }
        }
    }
    for (int i = 0; i < r; i++) {
        for (int j = 0; j < c; j++) {
            mat[i * c + j] = p1[i * c + j];
        }
    }
    free(p1);
    return mat;
}
