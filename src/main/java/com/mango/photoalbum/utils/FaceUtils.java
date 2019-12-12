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

    private static final String DOMAIN = "face.%s.aliyuncs.com";

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
     * 添加图片
     * @param faceInfo
     * @return
     */
    public String addFace(FaceInfo faceInfo) {
        try {
            log.info(JSONObject.toJSONString(faceInfo));
            CommonRequest request = new CommonRequest();
            request.setMethod(MethodType.POST);
            request.setDomain("face.cn-shanghai.aliyuncs.com");
            request.setVersion("2018-12-03");
            request.setAction(ACTION_ADD_FACE);
            putBodyParameter(request, faceInfo);
            CommonResponse response = face.getCommonResponse(request);
            return response.getData();
        } catch (ClientException | IllegalAccessException e) {
            e.printStackTrace();
            log.error("添加face发生错误:{}", e.getMessage());
        }
        return "";
    }

    private String getDomain() {
        return String.format(DOMAIN, regionId);
    }

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
