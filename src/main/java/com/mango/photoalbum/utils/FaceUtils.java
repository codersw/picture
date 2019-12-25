package com.mango.photoalbum.utils;

import com.alibaba.fastjson.JSONObject;
import com.aliyuncs.CommonRequest;
import com.aliyuncs.CommonResponse;
import com.aliyuncs.DefaultAcsClient;
import com.aliyuncs.exceptions.ClientException;
import com.aliyuncs.http.MethodType;
import com.aliyuncs.profile.DefaultProfile;
import com.mango.photoalbum.model.FaceInfo;
import com.mango.photoalbum.model.UploadFileFace;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import javax.annotation.PostConstruct;
import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

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

    @Value("${alibaba.face.regionId:cn-shanghai}")
    private String regionId;

    @Value("${alibaba.face.version:2018-12-03}")
    private String version;

    @Value("${alibaba.face.domain:face.cn-shanghai.aliyuncs.com}")
    private String domain;

    private DefaultAcsClient face;

    private static final String ACTION_ADD_FACE = "AddFace";

    private static final String ACTION_DELETE_FACE = "DeleteFace";

    private static final String ACTION_LIST_FACE = "ListFace";

    private static final String ACTION_LIST_GROUP = "ListGroup";

    private static final String ACTION_RECOGNIZE_FACE = "RecognizeFace";

    @PostConstruct
    private void init() {
        try {
            DefaultProfile profile = DefaultProfile.getProfile(regionId, accessKeyId, accessKeySecret);
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
            request.setDomain(domain);
            request.setVersion(version);
            request.setAction(ACTION_ADD_FACE);
            request.putBodyParameter("Group", faceInfo.getGroup()); //添加人脸的分组
            request.putBodyParameter("Person", faceInfo.getPerson()); //添加人脸的姓名
            request.putBodyParameter("Image", faceInfo.getImage());   //添加人脸的编号
            if(!StringUtils.isEmpty(faceInfo.getImageUrl())) {
                request.putBodyParameter("ImageUrl", faceInfo.getImageUrl()); //检测图片的URL
            }
            if(!StringUtils.isEmpty(faceInfo.getContent())) {
                request.putBodyParameter("Content", faceInfo.getContent()); //检测图片的内容，Base64编码
            }
            CommonResponse response = face.getCommonResponse(request);
            log.info("添加face成功:{}", response.getData());
        } catch (ClientException e) {
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
            request.setDomain(domain);
            request.setVersion(version);
            request.setAction(ACTION_DELETE_FACE);
            request.putBodyParameter("Group", faceInfo.getGroup()); //添加人脸的分组
            request.putBodyParameter("Person", faceInfo.getPerson()); //添加人脸的姓名
            request.putBodyParameter("Image", faceInfo.getImage());   //添加人脸的编号
            CommonResponse response = face.getCommonResponse(request);
            log.info("删除face成功:{}", response.getData());
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("删除face发生错误:{}", e.getMessage());
        }
    }

    /**
     * 列举注册库中的人脸
     * @param groupName 需要查询的库
     */
    public Map listFace(String groupName) {
        try {
            CommonRequest request = new CommonRequest();
            request.setMethod(MethodType.POST);
            request.setDomain(domain);
            request.setVersion(version);
            request.setAction(ACTION_LIST_FACE);
            request.putBodyParameter("Group", groupName);
            CommonResponse response = face.getCommonResponse(request);
            log.info("列举注册库中的face成功:{}", response.getData());
            JSONObject json = JSONObject.parseObject(response.getData());
            return JSONObject.parseObject(json.getString("Data"), Map.class);
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("列举注册库中的face发生错误:{}", e.getMessage());
        }
        return null;
    }

    /**
     * 接口用于列举人脸组
     */
    public List<String> listGroup() {
        try {
            CommonRequest request = new CommonRequest();
            request.setMethod(MethodType.POST);
            request.setDomain(domain);
            request.setVersion(version);
            request.setAction(ACTION_LIST_GROUP);
            CommonResponse response = face.getCommonResponse(request);
            log.info("列举人脸组成功:{}", response.getData());
            JSONObject json = JSONObject.parseObject(response.getData());
            return JSONObject.parseArray(json.getString("Data"), String.class);
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("列举人脸组发生错误:{}", e.getMessage());
        }
        return null;
    }

    /**
     * 接口用于查找注册库中的人脸
     * @param faceInfo
     * @return
     */
    public List<UploadFileFace> recognizeFace(FaceInfo faceInfo) {
        try {
            CommonRequest request = new CommonRequest();
            request.setMethod(MethodType.POST);
            request.setDomain(domain);
            request.setVersion(version);
            request.setAction(ACTION_RECOGNIZE_FACE);
            request.putBodyParameter("Group", faceInfo.getGroup());
            request.putBodyParameter("ImageUrl", faceInfo.getImageUrl());
            CommonResponse response = face.getCommonResponse(request);
            log.info("查找注册库中的人脸成功:{}", response.getData());
            JSONObject json = JSONObject.parseObject(response.getData());
            return JSONObject.parseArray(json.getString("Data"), UploadFileFace.class);
        } catch (ClientException e) {
            e.printStackTrace();
            log.error("查找注册库中的人脸发生错误:{}", e.getMessage());
            throw new RuntimeException("查找注册库中的人脸发生错误:" +  e.getMessage());
        }
    }

}
