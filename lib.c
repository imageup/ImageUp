#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
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

double* saturation_c(double *image, double saturation, double rowss, double colss) {
    int h = (int)rowss; //20
    int w = (int)colss; //30

    double *data =(double *) malloc((3 * h * w) * sizeof(double));
    double *output =(double *) malloc((3 * IMAGE_SIZE * IMAGE_SIZE) * sizeof(double));
    for (int i = 0; i < w; i++) {
        for (int j = 0; j < h; j++) {
            data[3*(w*j+i)] = image[3*(IMAGE_SIZE*j+i)];
            data[3*(w*j+i)+1] = image[3*(IMAGE_SIZE*j+i)+1];
            data[3*(w*j+i)+2] = image[3*(IMAGE_SIZE*j+i)+2];
        }
    }
    CvMat tmp_image = cvMat(h, w, CV_64FC3, data);
    cvSaveImage("tmp.jpg", &tmp_image, 0);
    CvMat *img = cvLoadImageM("tmp.jpg", CV_LOAD_IMAGE_COLOR);


    int rows = (int)rowss;
    int cols = (int)colss;
    IplImage *temp1=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    IplImage *temp2=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    cvCvtColor(img, temp1, CV_BGR2HSV);
    double tmp;
    unsigned char* input = (unsigned char*)(temp1->imageData);
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            tmp = saturation * input[img->step * i + j*3 + 1];
            if (tmp > 360) tmp = 360;
            if (tmp < 0) tmp = 0;
            input[img->step * i + j*3 + 1] = tmp ;
        }
    }
    cvCvtColor(temp1, temp2, CV_HSV2BGR);

    unsigned char* input2 = (unsigned char*)(temp2->imageData);
    int k = 0;
    for(int i = 0; i < IMAGE_SIZE; i++){
        for(int j = 0; j < IMAGE_SIZE; j++){
            if ( i >= rows || j >= cols ) {
                output[k++] = 0;
                output[k++] = 0;
                output[k++] = 0;
               } else {
                output[k++] = input2[img->step * i + j*3] ;
                output[k++] = input2[img->step * i + j*3 + 1];
                output[k++] = input2[img->step * i + j*3 + 2];
               }
            }
    }
    free(data);
    return output;
}

int string_to_int(char str[])
{
    int i;
    sscanf(str, "%d", &i);
    return i;
} 


double string_to_float(char s[])
{
     double val, power;
     int i, sign;
     for (i = 0; isspace(s[i]); i++) /* skip white space */
          ;

     sign = (s[i] == '-') ? -1 : 1;

     if (s[i] == '+' || s[i] == '-')
          i++;

     for (val = 0.0; isdigit(s[i]); i++)
          val = 10.0 * val + (s[i] - '0');

     if (s[i] == '.')
          i++;
     for (power = 1.0; isdigit(s[i]); i++) 
     {
          val = 10.0 * val + (s[i] - '0');
          power *= 10;
     }

     return sign * val / power;
}


char * int_to_string(int number)
{
    char *buff = malloc(10);
    snprintf (buff, sizeof(buff), "%d",number);
    return buff;
}


char * float_to_string(double number)
{
    char *buff = malloc(10);
    snprintf (buff, sizeof(buff), "%f",number);
    return buff;
}

char * string_concact(char str1[], char str2[])
{
    char* dest = malloc(40);
    dest[0]='\0';
    strcat(dest, str1);
    strcat(dest, str2);
    return dest;
}


double *smooth_c(double *image, double *pos, double rowss, double colss) {
    //CvMat *img = cvLoadImageM("./images/face1.jpg", CV_LOAD_IMAGE_COLOR);
   //xIplImage *dst=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);

    double r,g,b;
    int h = (int)rowss; //20
    int w = (int)colss; //30

    double *data =(double *) malloc((3 * h * w) * sizeof(double));
    double *output =(double *) malloc((3 * IMAGE_SIZE * IMAGE_SIZE) * sizeof(double));
    for (int i = 0; i < w; i++) {
        for (int j = 0; j < h; j++) {
            data[3*(w*j+i)] = image[3*(IMAGE_SIZE*j+i)];
            data[3*(w*j+i)+1] = image[3*(IMAGE_SIZE*j+i)+1];
            data[3*(w*j+i)+2] = image[3*(IMAGE_SIZE*j+i)+2];
        }
    }
    CvMat tmp_image = cvMat(h, w, CV_64FC3, data);
    cvSaveImage("tmp.jpg", &tmp_image, 0);
    CvMat *img = cvLoadImageM("tmp.jpg", CV_LOAD_IMAGE_COLOR);

    IplImage *temp1=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    IplImage *temp2=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    IplImage *temp3=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    IplImage *temp4=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    IplImage *dst=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    //CvMat dst = cvMat(h, w, CV_8UC3, data);

    int rows = (int)rowss; //20
    int cols = (int)colss; //30


    int value1 = 3.5, value2 = 1;   

    int dx = value1 * 5;   
    double fc = value1*12.5; 
    int p = 95;  


    cvSmooth(img, temp1, CV_BILATERAL, dx, dx, fc, fc);
    cvAddWeighted(temp1, 1.0,img, -1.0, 138.0, temp2);

    cvSmooth(temp2, temp3, CV_GAUSSIAN, 2 * value2 - 1, 2 * value2 - 1, 0, 0);

    cvAddWeighted(img, 1.0, temp3, 2.0, -255.0, temp4);
    cvAddWeighted(img, (100-p)/100.0, temp4, p/100.0, 0.0, dst);


    unsigned char* input = (unsigned char*)(dst->imageData);
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
    free(data);
    //CvMat dst = cvMat(rows, cols, CV_64FC3, data);
    return output;

}

void adjust_image_c(double *img, double *meta) {
    double alpha = meta[0];
    double beta = meta[1];
    int tmp = 0;
    for (int i = 0; i < IMAGE_SIZE; i++) {
        for (int j = 0; j < IMAGE_SIZE; j++) {
            for (int k = 0; k < 3; k++) {
                tmp = alpha * img[3 * (i * IMAGE_SIZE + j) + k] + beta;
                if (tmp > 255)
                    tmp = 255;
                if (tmp < 0)
                    tmp = 0;
                img[3 * (i * IMAGE_SIZE + j) + k] = tmp;
            }
        }
    }
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
