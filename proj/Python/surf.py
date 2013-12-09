import cv2, numpy
from PIL import Image


def stack_files(path, limit=None):
    import os, sys
    out_file = open('%s_stack.csv' % path, 'w')
    for file_count, csv_name in enumerate(os.listdir(path)):
        if csv_name not in ['.DS_Store']:
            inf = open(os.path.join(path, csv_name), 'r')
            for c, line in enumerate(inf):
                out_file.write(line.strip())
            inf.close()
            print >> out_file, ""
        if limit is not None and file_count >= limit: break
        if file_count % 100 == 0: sys.stderr.write("Procesados %i archivos de %s\r" % (file_count, path))
    out_file.close()



def process_folder(path, out_path, limit=None):
    import os, sys
    for c, img_name in enumerate(os.listdir(path)):
        if img_name.endswith(".jpg"):
            simple_name = img_name.split('.')
            simple_name = simple_name[0] if len(simple_name) == 2 else '_'.join([simple_name[0], simple_name[1]])
            out = process_image(os.path.join(path, img_name))
            out_file = os.path.join(out_path, simple_name)
            numpy.savetxt( out_file, out, delimiter=",", fmt='%i' )
        if c and c % 100 == 0: sys.stderr.write('Procesadas %i imagenes\r' % c)
        if limit is not None and c >= limit: break
    print >> sys.stderr, ''




def process_image(image_path, demo=False):
    
    img = cv2.imread( image_path )

    if demo:
        cv2.imshow("original", img)
        cv2.waitKey(0); cv2.destroyAllWindows()

    img = img[20:img.shape[0]-12, 20:img.shape[1]-5]
    
    if demo:
        cv2.imshow("original", img)
        cv2.waitKey(0); cv2.destroyAllWindows()

    white = numpy.ones_like(img)

    surfDetector = cv2.FeatureDetector_create("SURF")
    surfDescriptorExtractor = cv2.DescriptorExtractor_create("SURF")
    keypoints = surfDetector.detect(img)
    (keypoints, descriptors) = surfDescriptorExtractor.compute(img, keypoints)
    for kp in keypoints:
        x = int(kp.pt[0])
        y = int(kp.pt[1])
        cv2.circle(img, (x, y), 1, (0, 0, 255))
        cv2.circle(white, (x, y), 1, (0, 0, 255))

    imgray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    ret, thresh = cv2.threshold(imgray, 127, 255, 0)
    contours, hierarchy = cv2.findContours(thresh, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

    if len(contours) > 0:
        shapes = [(cont.shape[0], i) for i, cont in enumerate(contours)]
        max_contour = max(shapes)
        cv2.drawContours(img, contours[max_contour[1]], -1, (0, 255, 0), 2)
        cv2.drawContours(white, contours[max_contour[1]], -1, (0, 255, 0), 2)

    if demo:
        cv2.imshow("contours", img)
        cv2.waitKey(0); cv2.destroyAllWindows()

    if demo:
        cv2.imshow("blanca", white)
        cv2.waitKey(0); cv2.destroyAllWindows()

    white = cv2.resize(white, (64, 64))

    rotated = numpy.rot90( numpy.asarray(white), 3 )
    array = Image.fromarray(rotated).convert('P', dither=None)
    #numpy.savetxt("componentes.csv", array, delimiter=",", fmt='%i')
    return array



## =====================================================================================
## =====================================================================================
if __name__ == '__main__': ## dog.117.jpg
    process_image("dog_train/dog.112.jpg", demo=True) # 112
    #process_image("cat_train/cat.483.jpg", demo=True)
    #process_folder('cat_train', 'cat_components', limit=5000)
    #stack_files('dog_components')
    #process_folder('fix_test1', 'test_components', limit=5000)


