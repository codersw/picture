package com.mango.photoalbum;

import com.aliyuncs.CommonRequest;
import com.aliyuncs.CommonResponse;
import com.aliyuncs.DefaultAcsClient;
import com.aliyuncs.exceptions.ClientException;
import com.aliyuncs.http.MethodType;
import com.aliyuncs.profile.DefaultProfile;

public class Demo {

    //DefaultProfile.getProfile的参数分别是地域，access_key_id, access_key_secret
    private static DefaultProfile profile = DefaultProfile.getProfile("cn-shanghai", "LTAI4Fd66vvuVj31seQ2YbQQ", "AgUkSMWtlICI7lY2oBFM0aD94ZlqYt");
    private static DefaultAcsClient client = new DefaultAcsClient(profile);

    public static void main(String[] args) throws ClientException {
        String groupName = "JavaDemo1";
        String person = "LiuYifei";
        String image_1 = "photo1";
        String image_2 = "photo2";
        String imageUrl_1 = "https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1559655604341&di=3d6995f6dee1c4795d1827e754a00452&imgtype=0&src=http%3A%2F%2Fimg0.ph.126.net%2F90u9atgu46nnziAm1NMAGw%3D%3D%2F6631853916514183512.jpg";
        String imageUrl_2 = "https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1559655604338&di=ee3d8fb39f6e14a21852a4ac3f2c5a14&imgtype=0&src=http%3A%2F%2Fc4.haibao.cn%2Fimg%2F600_0_100_0%2F1473652712.0005%2F87c7805c10e60e9a6db94f86d6014de8.jpg";
        String recognizeFaceImageUrl = "https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1559655604335&di=7b540d703955aed6d235752589aee34a&imgtype=0&src=http%3A%2F%2Fphotocdn.sohu.com%2F20140317%2FImg396736687.jpg";

        //添加入两张人脸
        AddFace(groupName,person,image_1,imageUrl_1);
//
//        AddFace(groupName,person,image_2,imageUrl_2);
//
//        //列举Group
//        ListGroup();
//
//        //列举Faces
//        ListFace(groupName);
//
//        //人脸查询
//        RecognizeFace(recognizeFaceImageUrl, groupName);
//
//        //删除Face
//        DeleteFace(groupName,person,image_1);
//
//        //列举Faces查询删除情况
//        ListFace(groupName);
    }

    /**
     * AddFace接口用于向人脸库中添加人脸
     * @param groupName 添加人脸的分组
     * @param person 添加人脸的姓名
     * @param image 添加人脸的编号
     * @param imageUrl 检测图片的URL
     */
    public static void AddFace(String groupName,String person,String image,String imageUrl)
    {
        CommonRequest request = new CommonRequest();
        request.setMethod(MethodType.POST);
        request.setDomain("face.cn-shanghai.aliyuncs.com");
        request.setVersion("2018-12-03");
        request.setAction("AddFace");
        request.putBodyParameter("Group", groupName);
        request.putBodyParameter("Person", person);
        request.putBodyParameter("Image", image);
        request.putBodyParameter("ImageUrl",imageUrl);
//        request.putBodyParameter("Content", "/9j/4AAQSkZJRgABA...");  //检测图片的内容，Base64编码
        CommonResponse response = null;
        try {
            response = client.getCommonResponse(request);
        } catch (ClientException e) {
            e.printStackTrace();
        }
        System.out.println(response.getData());
    }

    /**
     * DeleteFace接口用于从人脸库中删除人脸
     * @param groupName 添加人脸的分组
     * @param person 添加人脸的姓名
     * @param image 添加人脸的编号
     * @throws ClientException
     */
    public static void DeleteFace(String groupName,String person,String image) throws ClientException {
        CommonRequest request = new CommonRequest();
        request.setMethod(MethodType.POST);
        request.setDomain("face.cn-shanghai.aliyuncs.com");
        request.setVersion("2018-12-03");
        request.setAction("DeleteFace");
        request.putBodyParameter("Group", groupName);
        request.putBodyParameter("Person", person);
        request.putBodyParameter("Image", image);
        CommonResponse response = client.getCommonResponse(request);
        System.out.println(response.getData());
    }

    /**
     * ListFace接口用于列举注册库中的人脸
     * @param groupName 需要查询的库
     * @throws ClientException
     */
    public static void ListFace(String groupName) throws ClientException {
        CommonRequest request = new CommonRequest();
        request.setMethod(MethodType.POST);
        request.setDomain("face.cn-shanghai.aliyuncs.com");
        request.setVersion("2018-12-03");
        request.setAction("ListFace");
        request.putBodyParameter("Group", groupName);
        CommonResponse response = client.getCommonResponse(request);
        System.out.println(response.getData());
    }

    /**
     * ListGroup接口用于列举人脸组
     * @throws ClientException
     */
    public static void ListGroup() throws ClientException {
        CommonRequest request = new CommonRequest();
        request.setMethod(MethodType.POST);
        request.setDomain("face.cn-shanghai.aliyuncs.com");
        request.setVersion("2018-12-03");
        request.setAction("ListGroup");
        CommonResponse response = client.getCommonResponse(request);
        System.out.println(response.getData());
    }

    /**
     * RecognizeFace接口用于查找注册库中的人脸
     * @param recognizeFaceImageUrl 需要查询的人类图片URL
     * @throws ClientException
     */
    public static void RecognizeFace(String recognizeFaceImageUrl, String groupName) throws ClientException {
        CommonRequest request = new CommonRequest();
        request.setMethod(MethodType.POST);
        request.setDomain("face.cn-shanghai.aliyuncs.com");
        request.setVersion("2018-12-03");
        request.setAction("RecognizeFace");
        request.putBodyParameter("Group", groupName);
        request.putBodyParameter("ImageUrl", recognizeFaceImageUrl);
//        request.putBodyParameter("Content", "/9j/4AAQSkZJRgABA...");  //检测图片的内容，Base64编码
        CommonResponse response = client.getCommonResponse(request);
        System.out.println(response.getData());
    }
}
