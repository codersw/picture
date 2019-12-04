package com.mango.photoalbum.utils;

import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

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
}
