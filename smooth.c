//#include <opencv4/cv.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/core/core_c.h>
#include <opencv2/core/types_c.h>
#include <opencv2/imgproc/imgproc_c.h> 
#include <stdio.h>
int main( int argc, char** argv )
{

 IplImage *src1;

 int p[1];
 p[0] = CV_IMWRITE_JPEG_QUALITY;

 src1 = cvLoadImage("art.jpg", CV_LOAD_IMAGE_COLOR);
 if (src1==NULL)
 	printf("wrong\n");

 IplImage *dst=cvCreateImage(cvGetSize(src1), IPL_DEPTH_8U,3);

 cvSmooth(src1, dst, CV_GAUSSIAN, 3, 3, 5, 5);
 cvSaveImage("smooth.jpg", dst, p);

 return 0;
}