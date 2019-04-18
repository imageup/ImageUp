//#include <opencv4/cv.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/core/core_c.h>
#include <opencv2/core/types_c.h>
#include <opencv2/imgproc/imgproc_c.h> 
#include <stdio.h>
#define IMAGE_SIZE 1000
int main( int argc, char** argv )
{



    CvMat *img = cvLoadImageM("./images/google-chrome.jpg", CV_LOAD_IMAGE_COLOR);
    unsigned char* input = (unsigned char*)(img->data.ptr);
    double *output =(double *) malloc((3 * IMAGE_SIZE * IMAGE_SIZE + 2) * sizeof(double));

    double r,g,b;
    int rows = img->rows;
    int cols = img->cols;

    output[0] = rows;
    output[1] = cols;
    int k = 2;
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
    printf("%f %f %d", output[0], output[1], k);
    //free(output);
 /*IplImage *src1;

 int p[1];
 p[0] = CV_IMWRITE_JPEG_QUALITY;

 src1 = cvLoadImage("art.jpg", CV_LOAD_IMAGE_COLOR);


 IplImage *dst=cvCreateImage(cvGetSize(src1), IPL_DEPTH_8U,3);

 cvSmooth(src1, dst, CV_GAUSSIAN, 3, 3, 5, 5);
 cvSaveImage("smooth.jpg", dst, p);*/

 return 0;
}