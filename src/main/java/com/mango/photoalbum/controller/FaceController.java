package com.mango.photoalbum.controller;

import com.mango.photoalbum.model.*;
import com.mango.photoalbum.service.FaceService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import javax.annotation.Resource;

@Api(value = "人脸信息接口", tags = {"人脸信息接口"})
@Slf4j
@RestController
@RequestMapping("/face")
public class FaceController {

    @Resource
    private FaceService faceService;

    /**
     * 保存人脸库
     * @param faceInfoCo
     * @return
     */
    @ApiOperation(value = "保存人脸库", notes = "保存人脸库")
    @ApiImplicitParams({@ApiImplicitParam(paramType = "form", dataType="__file", name = "file",
            value = "文件", required = true)})
    @PostMapping(headers = "content-type=multipart/form-data")
    public Result save(@ModelAttribute FaceInfoCo faceInfoCo) {
        faceService.save(faceInfoCo);
        return ResultGenerator.genSuccessResult();
    }

    /**
     * 人脸库记录
     * @param faceInfoListCo
     * @return
     */
    @ApiOperation(value = "人脸库记录", notes = "人脸库记录")
    @GetMapping("/list")
    public Result list(FaceInfoListCo faceInfoListCo) {
        return ResultGenerator.genSuccessResult(PageResponse.<FaceInfo>builder()
                .total(faceService.total(faceInfoListCo))
                .list(faceService.list(faceInfoListCo))
                .build());
    }

    /**
     * 人脸库列表
     * @return
     */
    @ApiOperation(value = "人脸库列表", notes = "人脸库列表")
    @GetMapping("/listFace")
    public Result listFace() {
        return ResultGenerator.genSuccessResult(faceService.listFace());
    }
}
