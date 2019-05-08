//#include <opencv4/cv.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/core/core_c.h>
#include <opencv2/core/types_c.h>
#include <opencv2/imgproc/imgproc_c.h> 
#include <stdio.h>
#define IMAGE_SIZE 1000
int main( int argc, char** argv )
{



    CvMat *img = cvLoadImageM("./images/face2.jpg", CV_LOAD_IMAGE_COLOR);
   //xIplImage *dst=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    int rows = img->rows;
    int cols = img->cols;


    int value1 = 3, value2 = 1;   

    int dx = value1 * 5;   
    double fc = value1*12.5; 
    int p = 55;  
    IplImage *dst=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    IplImage *temp1=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    IplImage *temp2=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    IplImage *temp3=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);
    IplImage *temp4=cvCreateImage(cvGetSize(img), IPL_DEPTH_8U,3);

    cvSmooth(img, temp1, CV_BILATERAL, dx, dx, fc, fc);
    cvAddWeighted(temp1, 1.0, img, -1.0, 138.0, temp2);

    cvSmooth(temp2, temp3, CV_GAUSSIAN, 2 * value2 - 1, 2 * value2 - 1, 0, 0);

    cvAddWeighted(img, 1.0, temp3, 2.0, -255.0, temp4);
    cvAddWeighted(img, (100-p)/100.0, temp4, p/100.0, 0.0, dst);
    

    //CvMat dst = cvMat(rows, cols, CV_64FC3, data);
    cvSaveImage("smooth.jpg", dst, 0);



 return 0;
}