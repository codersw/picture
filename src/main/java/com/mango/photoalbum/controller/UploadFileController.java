package com.mango.photoalbum.controller;

import com.mango.photoalbum.annotation.ApiVersion;
import com.mango.photoalbum.enums.IsCoverEnum;
import com.mango.photoalbum.model.*;
import com.mango.photoalbum.service.PhotoAlbumService;
import com.mango.photoalbum.service.UploadFileService;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * 文件接口
 * @author swen
 */
@Api(value = "文件接口", tags = {"文件接口"})
@Slf4j
@RestController
@RequestMapping("/upload")
public class UploadFileController {

    @Resource
    private UploadFileService uploadFileService;

    @Resource
    private PhotoAlbumService photoAlbumService;

    /**
     * 上传图片
     * @param uploadFileCo
     * @return
     */
    @ApiOperation(value = "文件上传接口", notes = "文件上传接口")
    @ApiImplicitParams({@ApiImplicitParam(paramType = "form", dataType="__file", name = "file",
            value = "文件", required = true)})
    @PostMapping(value = "/file", headers = "content-type=multipart/form-data")
    public Result file(@ModelAttribute UploadFileCo uploadFileCo) {
        try {
            return ResultGenerator.genSuccessResult(uploadFileService.save(uploadFileCo));
        } catch (Exception e){
            e.printStackTrace();
            log.error("上传文件发生异常{}", e.getMessage());
            return ResultGenerator.genFailResult("上传文件发生异常");
        }
    }

    /**
     * 文件详情修改接口
     * @param uploadFileUpdateCo
     * @return
     */
    @ApiOperation(value = "修改文件接口", notes = "修改文件接口")
    @PostMapping(value = "/file/update")
    public Result fileUpdate(@RequestBody UploadFileUpdateCo uploadFileUpdateCo) {
        try {
            uploadFileService.update(uploadFileUpdateCo);
            return ResultGenerator.genSuccessResult();
        } catch (Exception e){
            e.printStackTrace();
            log.error("修改文件发生异常{}", e.getMessage());
            return ResultGenerator.genFailResult("修改文件发生异常");
        }
    }

    /**
     * 上传图片
     * @param uploadFileMultiCo
     * @return
     */
    @ApiOperation(value = "文件批量上传接口", notes = "swagger批量文件上传不好用请用postman等工具测试")
    @PostMapping(value = "/files", headers = "content-type=multipart/form-data")
    public Result files(@ModelAttribute UploadFileMultiCo uploadFileMultiCo) {
        try {
            return ResultGenerator.genSuccessResult(uploadFileService.save(uploadFileMultiCo));
        } catch (Exception e){
            e.printStackTrace();
            log.error("上传文件发生异常{}", e.getMessage());
            return ResultGenerator.genFailResult(e.getMessage());
        }
    }

    /**
     * 相册详情
     * @param fileId
     * @return
     */
    @ApiOperation(value = "文件详情", notes = "相册详情")
    @GetMapping("/{fileId}")
    public Result get(@PathVariable @ApiParam("文件id") String fileId) {
        return ResultGenerator.genSuccessResult(uploadFileService.get(fileId));
    }

    /**
     * 删除相册
     * @param fileId
     * @return
     */
    @ApiOperation(value = "删除文件", notes = "删除文件")
    @DeleteMapping("/{fileId}")
    public Result delete(@PathVariable @ApiParam("文件id") String fileId) {
        uploadFileService.delete(fileId);
        return ResultGenerator.genSuccessResult();
    }

    /**
     * 相册列表
     * @return
     */
    @ApiOperation(value = "文件列表", notes = "文件列表")
    @GetMapping("/list")
    public Result list(UploadFileListCo uploadFileListCo) {
        uploadFileListCo.setTotal(uploadFileService.total(uploadFileListCo));
        List<UploadFile> fileList = uploadFileService.list(uploadFileListCo);
        PhotoAlbum photoAlbum = photoAlbumService.get(uploadFileListCo.getAlbumId());
        fileList.forEach(file ->{
            file.setIsCover(IsCoverEnum.FALSE.getValue());
            if(file.getFileId().equals(photoAlbum.getCover())) {
                file.setIsCover(IsCoverEnum.TRUE.getValue());
            }
        });
        return ResultGenerator.genSuccessResult(PageResponse.<UploadFile>builder()
                .total(uploadFileListCo.getTotal())
                .list(fileList)
                .data(photoAlbum)
                .build());
    }

    /**
     * 下载文件
     * @param fileId
     * @param response
     */
    @ApiOperation(value = "下载文件", notes = "文件文件")
    @RequestMapping(value = "/download/{fileId}" , method = { RequestMethod.GET, RequestMethod.POST})
    public void download(@PathVariable @ApiParam("文件id") String fileId, HttpServletResponse response) {
        try {
            uploadFileService.download(fileId, response);
        } catch (Exception e) {
            e.printStackTrace();
            log.error("下载文件发生异常{}", e.getMessage());
        }
    }

}
