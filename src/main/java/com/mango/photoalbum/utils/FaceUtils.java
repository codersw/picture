package com.mango.photoalbum.utils;


import com.alibaba.fastjson.JSONObject;
import com.aliyuncs.CommonRequest;
import com.aliyuncs.CommonResponse;
import com.aliyuncs.DefaultAcsClient;
import com.aliyuncs.exceptions.ClientException;
import com.aliyuncs.http.MethodType;
import com.aliyuncs.profile.DefaultProfile;
import com.mango.photoalbum.model.FaceInfo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.lang.reflect.Field;
import java.util.Date;

/**
 * face工具类
 *@Author swen
 */
@Slf4j
@Component
public class FaceUtils {

    @Value("${alibaba.face.accessKeyId}")
    private String accessKeyId;

    @Value("${alibaba.face.accessKeySecret}")
    private String accessKeySecret;

    @Value("${alibaba.face.regionId}")
    private String regionId;

    private DefaultAcsClient face;

    private static final String ACTION_ADD_FACE = "AddFace";

    private static final String ACTION_DELETE_FACE = "DeleteFace";

    private static final String ACTION_LIST_FACE = "ListFace";

    private static final String ACTION_LIST_GROUP = "ListGroup";

    private static final String ACTION_RECOGNIZE_FACE = "RecognizeFace";

    private static final String DOMAIN = "face.cn-shanghai.aliyuncs.com";

    private static final String VERSION = "2018-12-03";

    //@PostConstruct
    private void init() {
        try {
            DefaultProfile profile = DefaultProfile.getProfile("cn-shanghai", accessKeyId, accessKeySecret);
            face = new DefaultAcsClient(profile);
        } catch (Exception e){
            e.printStackTrace();
            face = null;
            log.error("初始化face出错:{}", e.getMessage());
        }
    }

    /**
     * 添加face
     * @param faceInfo
     * @return
     */
    public void addFace(FaceInfo faceInfo) {
        try {
            log.info(JSONObject.toJSONString(faceInfo));
            CommonRequest request = new CommonRequest();
            request.setMethod(MethodType.POST);
            request.setDomain(DOMAIN);
            request.setVersion(VERSION);
            request.setAction(ACTION_ADD_FACE);
            putBodyParameter(request, faceInfo);
            CommonResponse response = face.getCommonResponse(request);
            log.info("添加face成功:{}", response.getData());
        } catch (ClientException | IllegalAccessException e) {
            e.printStackTrace();
            log.error("添加face发生错误:{}", e.getMessage());
        }
    }

    /**
     * 删除face
     * @param faceInfo
     */
    public void deleteFace(FaceInfo faceInfo) {
        try {
            CommonRequest request = new CommonRequest();
            request.setMethod(MethodType.POST);
            request.setDomain(DOMAIN);
            request.setVersion(VERSION);
            request.setAction(ACTION_DELETE_FACE);
            putBodyParameter(request, faceInfo);
            CommonResponse response = face.getCommonResponse(request);
            log.info("删除face成功:{}", response.getData());
        } catch (ClientException | IllegalAccessException e) {
            e.printStackTrace();
            log.error("删除face发生错误:{}", e.getMessage());
        }
    }

    /**
     * 列举注册库中的人脸
     * @param groupName 需要查询的库
     */
    public void listFace(String groupName) {
        try {
            CommonRequest request = new CommonRequest();
            request.setMethod(MethodType.POST);
            request.setDomain(DOMAIN);
            request.setVersion(VERSION);
            request.setAction(ACTION_LIST_FACE);
            request.putBodyParameter("Group", groupName);
            CommonResponse response = face.getCommonResponse(request);
            log.info("注册库中的face成功:{}", response.getData());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("注册库中的face发生错误:{}", e.getMessage());
        }
    }

    /**
     * 接口用于列举人脸组
     */
    public void listGroup() {
        try {
            CommonRequest request = new CommonRequest();
            request.setMethod(MethodType.POST);
            request.setDomain(DOMAIN);
            request.setVersion(VERSION);
            request.setAction(ACTION_LIST_GROUP);
            CommonResponse response = face.getCommonResponse(request);
            log.info("列举人脸组成功:{}", response.getData());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("列举人脸组发生错误:{}", e.getMessage());
        }
    }

    /**
     * 接口用于查找注册库中的人脸
     * @param recognizeFaceImageUrl 需要查询的人类图片URL
     */
    public void recognizeFace(String recognizeFaceImageUrl, String groupName) {
        try {
            CommonRequest request = new CommonRequest();
            request.setMethod(MethodType.POST);
            request.setDomain(DOMAIN);
            request.setVersion(VERSION);
            request.setAction(ACTION_RECOGNIZE_FACE);
            request.putBodyParameter("Group", groupName);
            request.putBodyParameter("ImageUrl", recognizeFaceImageUrl);
            CommonResponse response = face.getCommonResponse(request);
            log.info("查找注册库中的人脸成功:{}", response.getData());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("查找注册库中的人脸发生错误:{}", e.getMessage());
        }
    }

    /**
     * 格式化参数
     * @param request
     * @param faceInfo
     * @throws IllegalAccessException
     */
    private void putBodyParameter(CommonRequest request, FaceInfo faceInfo) throws IllegalAccessException {
        Field[] fields = faceInfo.getClass().getDeclaredFields();
        for(Field field : fields) {
            field.setAccessible(true);
            String name = field.getName(); // 获取属性的名字
            Object value = field.get(faceInfo);//获取属性值
            if(!CommonUtils.isNullOrEmpty(value)){
                request.putBodyParameter(name, value);
            }
        }
    }
}
