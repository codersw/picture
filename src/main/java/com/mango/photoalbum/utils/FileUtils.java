package com.mango.photoalbum.utils;

import com.drew.imaging.ImageMetadataReader;
import com.drew.imaging.ImageProcessingException;
import com.drew.metadata.Directory;
import com.drew.metadata.Metadata;
import com.drew.metadata.exif.ExifIFD0Directory;
import com.drew.metadata.exif.ExifSubIFDDirectory;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * file工具类
 * @Author swen
 */
public class FileUtils extends org.apache.commons.io.FileUtils {

    /**
     * 文件上传
     * @param file
     * @param uploadPath
     * @return
     * @throws Exception
     */
    public static String uploadFile(MultipartFile file, String uploadPath) throws Exception {
        mkdirIfNotExits(uploadPath);
        String fileName = file.getOriginalFilename();
        String uuid= CommonUtils.UUID();
        //创建输出文件对象
        File outFile = new File(uploadPath  + uuid + getFileType(fileName));
        //拷贝文件到输出文件对象
        file.transferTo(outFile);
        return uuid + getFileType(fileName);
    }

    /**
     * 获取文件后缀名
     * @param fileName
     * @return
     */
    public static String getFileType(String fileName) {
        if(fileName!=null && fileName.contains(".")) {
            return fileName.substring(fileName.lastIndexOf("."), fileName.length());
        }
        return "";
    }

    /**
     * 转图片对象
     * @param file
     * @return
     */
    public static BufferedImage toImage(MultipartFile file) throws IOException {
        return ImageIO.read(file.getInputStream());
    }

    /**
     * 创建目录
     * @param filePath
     */
    private static void mkdirIfNotExits(String filePath) {
        File file = new File(filePath);
        if (!file.exists()) {
            file.mkdirs();
        }
    }

    /**
     * 创建文件夹
     * @param filePath
     * @throws IOException
     */
    private static void createFileIfNotExits(String filePath) throws IOException {
        File file = new File(filePath);
        if (!file.exists()) {
            file.createNewFile();
        }
    }

    /**
     * 通过文件名判断并获取文件的contentType
     * @param fileName 文件名
     * @return 文件的contentType
     */
    public static String getContentType(String fileName) {
        String fileExtension = fileName.substring(fileName.lastIndexOf("."));
        if (".bmp".equalsIgnoreCase(fileExtension))
            return "image/bmp";
        if (".gif".equalsIgnoreCase(fileExtension))
            return "image/gif";
        if (".jpeg".equalsIgnoreCase(fileExtension))
            return "image/jpeg";
        if (".jpg".equalsIgnoreCase(fileExtension))
            return "image/jpg";
        if (".png".equalsIgnoreCase(fileExtension))
            return "image/png";
        if (".html".equalsIgnoreCase(fileExtension))
            return "text/html";
        if (".txt".equalsIgnoreCase(fileExtension))
            return "text/plain";
        if (".vsd".equalsIgnoreCase(fileExtension))
            return "application/vnd.visio";
        if (".ppt".equalsIgnoreCase(fileExtension) || "pptx".equalsIgnoreCase(fileExtension))
            return "application/vnd.ms-powerpoint";
        if (".doc".equalsIgnoreCase(fileExtension) || "docx".equalsIgnoreCase(fileExtension))
            return "application/msword";
        if (".xml".equalsIgnoreCase(fileExtension))
            return "text/xml";
        return "text/html";
    }

    /**
     * 图片属性
     * @param file
     * @return
     */
    public static Map<String, Object> getImgInfo(InputStream file) {
        Map<String, Object> result = new HashMap<>();
        try {
            Metadata metadata = ImageMetadataReader.readMetadata(file);
            for (Directory directory : metadata.getDirectories()) {
                if("ExifSubIFDDirectory".equalsIgnoreCase(directory.getClass().getSimpleName())){
                    //光圈F值=镜头的焦距/镜头光圈的直径
                    result.put("fnumber", directory.getString(ExifSubIFDDirectory.TAG_FNUMBER));
                    //曝光时间秒
                    result.put("exposure_time", directory.getString(ExifSubIFDDirectory.TAG_EXPOSURE_TIME));
                    //ISO速度
                    result.put("iso_equivalent", directory.getString(ExifSubIFDDirectory.TAG_ISO_EQUIVALENT));
                    //焦距毫米
                    result.put("facal_length", directory.getString(ExifSubIFDDirectory.TAG_FOCAL_LENGTH));
                    //拍照时间
                    result.put("tdatetime_original", directory.getString(ExifSubIFDDirectory.TAG_DATETIME_ORIGINAL));
                    //宽
                    result.put("width", directory.getString(ExifSubIFDDirectory.TAG_EXIF_IMAGE_WIDTH));
                    //高
                    result.put("height", directory.getString(ExifSubIFDDirectory.TAG_EXIF_IMAGE_HEIGHT));
                }
                if("ExifIFD0Directory".equalsIgnoreCase(directory.getClass().getSimpleName())){
                    //照相机制造商
                    result.put("make", directory.getString(ExifIFD0Directory.TAG_MAKE));
                    //照相机型号
                    result.put("model", directory.getString(ExifIFD0Directory.TAG_MODEL));
                    //水平分辨率
                    result.put("x_resolution", directory.getString(ExifIFD0Directory.TAG_X_RESOLUTION));
                    //垂直分辨率
                    result.put("y_resolution", directory.getString(ExifIFD0Directory.TAG_Y_RESOLUTION));
                }
            }
        } catch (ImageProcessingException | IOException e) {
            e.printStackTrace();
        }
        return result;
    }
}
