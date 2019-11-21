package com.mango.picture.utils;

import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSClientBuilder;
import com.aliyun.oss.model.OSSObject;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.OutputStream;


@Slf4j
@Data
@Component
public class OssUtils {

    private static String accessKeyId;

    private static String accessKeySecret;

    private static String endpoint;

    private static String bucketName;

    private static String httpPrefix;

    private static OSS oss;

    @Value("${alibaba.oss.accessKeyId}")
    public void setAccessKeyId(String accessKeyId) {
        OssUtils.accessKeyId = accessKeyId;
    }

    @Value("${alibaba.oss.accessKeySecret}")
    public void setAccessKeySecret(String accessKeySecret) {
        OssUtils.accessKeySecret = accessKeySecret;
    }

    @Value("${alibaba.oss.endpoint}")
    public void setEndpoint(String endpoint) {
        OssUtils.endpoint = endpoint;
    }

    @Value("${alibaba.oss.bucketName}")
    public void setBucketName(String bucketName) {
        OssUtils.bucketName = bucketName;
    }

    @Value("${alibaba.oss.httpPrefix}")
    public void setHttpPrefix(String httpPrefix) {
        OssUtils.httpPrefix = httpPrefix;
    }

    /**
     * 初始化OSS访问对象
     */
    static {
        try {
            oss = new OSSClientBuilder().build(endpoint, accessKeyId, accessKeySecret);
        } catch (Exception e){
            oss = null;
            e.printStackTrace();
            log.error("初始化oss出错{}", e.getMessage());
        }
    }

    /**
     * 保存文件
     * @param input
     * @param targetPath
     */
    public void save(InputStream input, String targetPath) {
        oss.putObject(bucketName, targetPath, input);
    }

    /**
     * 复制文件
     * @param sourcePath
     * @param targetPath
     * @throws Exception
     */
    public void copy(String sourcePath, String targetPath) throws Exception {
        boolean isExistInOss = oss.doesObjectExist(bucketName, sourcePath);
        if (!isExistInOss) {
            throw new FileNotFoundException("OSS没有找到" + sourcePath + "文件");
        }
        oss.copyObject(bucketName, sourcePath, bucketName, targetPath);
    }

    /**
     * 删除文件
     * @param filepath
     */
    public void delete(String filepath) {
        oss.deleteObject(bucketName, filepath);
    }

    /**
     * 读取文件
     * @param sourcePath
     * @param output
     * @throws Exception
     */
    public void load(String sourcePath, OutputStream output) throws Exception {
        OSSObject ossObject = oss.getObject(bucketName, sourcePath);
        IOUtils.copy(ossObject.getObjectContent(), output);
    }

    /**
     * 判断路径是否存在
     * @param path
     * @return
     */
    public boolean exists(String path) {
        return oss.doesObjectExist(bucketName, path);
    }

    /**
     * OSS的外部链接拼接规则
     * @param path
     * @return
     */
    public String getViewUrl(String path) {
        return httpPrefix + bucketName + "." + endpoint + "/" + path;
    }
}
