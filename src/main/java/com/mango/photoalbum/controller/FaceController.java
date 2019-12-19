package com.mango.photoalbum.controller;

import com.mango.photoalbum.model.FaceInfoCo;
import com.mango.photoalbum.model.Result;
import com.mango.photoalbum.model.ResultGenerator;
import com.mango.photoalbum.service.FaceService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import javax.annotation.Resource;

@Api(value = "脸部识别接口", tags = {"脸部识别接口"})
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
    public Result save(@RequestBody FaceInfoCo faceInfoCo) {
        faceService.save(faceInfoCo);
        return ResultGenerator.genSuccessResult();
    }

}
