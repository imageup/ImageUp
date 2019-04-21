#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/core/core_c.h>
#include <opencv2/core/types_c.h>
#include <opencv2/imgproc/imgproc_c.h> 
#define MATRIX_SIZE 200
#define IMAGE_SIZE 1000


double* dim_c(char path[]){

    CvMat *img = cvLoadImageM(path, CV_LOAD_IMAGE_COLOR);
    unsigned char* input = (unsigned char*)(img->data.ptr);
    double *output =(double *) malloc(2 * sizeof(double));


    int rows = img->rows;
    int cols = img->cols;
    output[0] = rows;
    output[1] = cols;
    return output;
}


double* read_c(char path[]){

    CvMat *img = cvLoadImageM(path, CV_LOAD_IMAGE_COLOR);
    unsigned char* input = (unsigned char*)(img->data.ptr);
    double *output =(double *) malloc((3 * IMAGE_SIZE * IMAGE_SIZE) * sizeof(double));

    double r,g,b;
    int rows = img->rows;
    int cols = img->cols;


    int k = 0;
    for(int i = 0; i < IMAGE_SIZE; i++){
        for(int j = 0; j < IMAGE_SIZE; j++){
            if ( i >= rows || j >= cols ) {
                b = 0;
                output[k++] = b;
                g = 0;
                output[k++] = g;
                r = 0;
                output[k++] = r;
               } else {
                b = input[img->step * i + j*3] ;
                output[k++]=b;
                g = input[img->step * i + j*3 + 1];
                output[k++]=g;
                r = input[img->step * i + j*3 + 2];
                output[k++]=r;
               }

        }
    }

    return output;
    
}

double *get_pixel_c(double *img, double *pos) {

    int row = (int) pos[0];
    int col = (int) pos[1];

    double *output =(double *) malloc(3 * sizeof(double));
    
    double b = img[3*(IMAGE_SIZE*row+col)];
    double g = img[3*(IMAGE_SIZE*row+col)+1];
    double r = img[3*(IMAGE_SIZE*row+col)+2];
    output[0] = b;
    output[1] = g;
    output[2] = r;



    return output;
}

void write_pixel_c(double *img, double *pos, double *value) {

    int row = (int) pos[0];
    int col = (int) pos[1];
    double r = value[0];
    double g = value[1];
    double b = value[2];
    img[3*(IMAGE_SIZE*row+col)] = b;
    img[3*(IMAGE_SIZE*row+col)+1] = g;
    img[3*(IMAGE_SIZE*row+col)+2] = r;

}

void save_c(char outname[], double *img, double rows, double cols) {

    int h = (int)rows; //20
    int w = (int)cols; //30
    double *data =(double *) malloc((3 * h * w) * sizeof(double));

    for (int i = 0; i < w; i++) {
        for (int j = 0; j < h; j++) {
            data[3*(w*j+i)] = img[3*(IMAGE_SIZE*j+i)];
            data[3*(w*j+i)+1] = img[3*(IMAGE_SIZE*j+i)+1];
            data[3*(w*j+i)+2] = img[3*(IMAGE_SIZE*j+i)+2];
        }
    }


    CvMat image = cvMat(h, w, CV_64FC3, data);
    cvSaveImage(outname,&image, 0);
    free(data);
    return;
}
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
        for (int i = 0; i < c; i++) {
            for (int j = 0; j < r; j++) {
                p1[(c - 1 - i) * MATRIX_SIZE + j] = mat[i * MATRIX_SIZE + j];
            }
        }
    }
    else {
        for (int i = 0; i < c; i++) {
            for (int j = 0; j < r; j++) {
                p1[i * MATRIX_SIZE + (r - 1 - j)] = mat[i * MATRIX_SIZE + j];
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
