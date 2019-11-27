package com.mango.photoalbum.utils;

import com.aliyun.oss.*;
import com.aliyun.oss.model.CannedAccessControlList;
import com.aliyun.oss.model.GeneratePresignedUrlRequest;
import com.aliyun.oss.model.OSSObject;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import javax.annotation.PostConstruct;
import java.io.*;
import java.util.Date;

/**
 * oss工具类
 *@Author swen
 */
@Slf4j
@Component
public class OssUtils {

    @Value("${alibaba.oss.accessKeyId}")
    private String accessKeyId;

    @Value("${alibaba.oss.accessKeySecret}")
    private String accessKeySecret;

    @Value("${alibaba.oss.endpoint}")
    private String endpoint;

    @Value("${alibaba.oss.bucketName}")
    private String bucketName;

    @Value("${alibaba.oss.httpPrefix}")
    private String httpPrefix;

    private OSS oss;

    /**
     * 初始化OSS访问对象
     */
    @PostConstruct
    private void init() {
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
     * 保存文件
     * @param input
     * @param targetPath
     * @throws IOException
     */
    public void saveAcl(InputStream input, String targetPath) throws IOException {
        oss.putObject(bucketName, targetPath, input);
        oss.setBucketAcl(bucketName, CannedAccessControlList.Private);
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

    /**
     * OSS的外部链接拼接规则
     * @param key
     * @param failureTime
     * @return
     * @throws OSSException
     * @throws ClientException
     */
    public String getViewUrlAcl(String key, Integer failureTime)  throws OSSException, ClientException{
        Date expiration = new Date(System.currentTimeMillis() + 1000 * failureTime);
        GeneratePresignedUrlRequest req = new GeneratePresignedUrlRequest(bucketName, key, HttpMethod.GET);
        req.setExpiration(expiration);
        return oss.generatePresignedUrl(req).toString();
    }
}
