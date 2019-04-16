#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#define MATRIX_SIZE 200
double* transpose_c(double* mat, int r, int c) {
    double *p1 =(double *) malloc(MATRIX_SIZE * MATRIX_SIZE * sizeof(double));
    for (int i = 0; i < MATRIX_SIZE; i++) {
        for (int j = 0; j < MATRIX_SIZE; j++) {
            p1[j * MATRIX_SIZE + i] = mat[i * MATRIX_SIZE + j];
        }
    }
    for (int i = 0; i < MATRIX_SIZE; i++) {
        for (int j = 0; j < MATRIX_SIZE; j++) {
            mat[i * MATRIX_SIZE + j] = p1[i * MATRIX_SIZE + j];
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

double* rotate_c(double* mat, int r, int c, bool dir) {
    mat = transpose_c(mat, r, c);
    double *p1 = (double *)malloc(MATRIX_SIZE * MATRIX_SIZE * sizeof(double));
    if (dir == true) {//left
        for (int i = 0; i < MATRIX_SIZE; i++) {
            for (int j = 0; j < MATRIX_SIZE; j++) {
                p1[(MATRIX_SIZE - 1 - i) * MATRIX_SIZE + j] = mat[i * MATRIX_SIZE + j];
            }
        }
    }
    else {
        for (int i = 0; i < MATRIX_SIZE; i++) {
            for (int j = 0; j < MATRIX_SIZE; j++) {
                p1[i * MATRIX_SIZE + (MATRIX_SIZE - 1 - j)] = mat[i * MATRIX_SIZE + j];
            }
        }
    }
    for (int i = 0; i < MATRIX_SIZE; i++) {
        for (int j = 0; j < MATRIX_SIZE; j++) {
            mat[i * MATRIX_SIZE + j] = p1[i * MATRIX_SIZE + j];
        }
    }
    free(p1);
    return mat;
}
