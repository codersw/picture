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
     * 文件上传接口
     * @param uploadFileCo
     * @return
     */
    @ApiOperation(value = "文件上传接口", notes = "文件上传接口")
    @ApiImplicitParams({@ApiImplicitParam(paramType = "form", dataType="__file", name = "file",
            value = "文件", required = true)})
    @PostMapping(value = "/file", headers = "content-type=multipart/form-data")
    public Result file(@ModelAttribute UploadFileCo uploadFileCo) {
        return ResultGenerator.genSuccessResult(uploadFileService.save(uploadFileCo));
    }

    /**
     * 文件上传接口
     * @param uploadFileCo
     * @return
     */
    @ApiVersion
    @ApiOperation(value = "文件上传接口", notes = "文件上传接口")
    @ApiImplicitParams({@ApiImplicitParam(paramType = "form", dataType="__file", name = "file",
            value = "文件", required = true)})
    @PostMapping(value = "/v1/file", headers = "content-type=multipart/form-data")
    public Result fileV1(@ModelAttribute UploadFileCo uploadFileCo) {
        return ResultGenerator.genSuccessResult(uploadFileService.saveV1(uploadFileCo));
    }

    /**
     * 修改文件接口
     * @param uploadFileUpdateCo
     * @return
     */
    @ApiOperation(value = "修改文件接口", notes = "修改文件接口")
    @PostMapping(value = "/file/update")
    public Result fileUpdate(@RequestBody UploadFileUpdateCo uploadFileUpdateCo) {
        uploadFileService.update(uploadFileUpdateCo);
        return ResultGenerator.genSuccessResult();
    }

    /**
     * 文件批量上传接口
     * @param uploadFileMultiCo
     * @return
     */
    @ApiOperation(value = "文件批量上传接口", notes = "swagger批量文件上传不好用请用postman等工具测试")
    @PostMapping(value = "/files", headers = "content-type=multipart/form-data")
    public Result files(@ModelAttribute UploadFileMultiCo uploadFileMultiCo) {
        return ResultGenerator.genSuccessResult(uploadFileService.save(uploadFileMultiCo));
    }

    /**
     * 文件批量上传接口
     * @param uploadFileMultiCo
     * @return
     */
    @ApiVersion
    @ApiOperation(value = "文件批量上传接口", notes = "swagger批量文件上传不好用请用postman等工具测试")
    @PostMapping(value = "/v1/files", headers = "content-type=multipart/form-data")
    public Result filesV1(@ModelAttribute UploadFileMultiCo uploadFileMultiCo) {
        return ResultGenerator.genSuccessResult(uploadFileService.saveV1(uploadFileMultiCo));
    }

    /**
     * 文件详情
     * @param fileId
     * @return
     */
    @ApiOperation(value = "文件详情", notes = "相册详情")
    @GetMapping("/{fileId}")
    public Result get(@PathVariable @ApiParam("文件id") String fileId) {
        return ResultGenerator.genSuccessResult(uploadFileService.get(fileId));
    }

    /**
     * 删除文件
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
     * 文件列表
     * @return
     */
    @ApiOperation(value = "文件列表", notes = "文件列表")
    @GetMapping("/list")
    public Result list(UploadFileListCo uploadFileListCo) {
        uploadFileListCo.setTotal(uploadFileService.total(uploadFileListCo));
        List<UploadFile> fileList = uploadFileService.list(uploadFileListCo);
        PhotoAlbum photoAlbum = photoAlbumService.get(uploadFileListCo.getAlbumId());
        fileList.stream()
                .filter(file -> file.getFileId().equals(photoAlbum.getCover()))
                .forEach(file -> file.setIsCover(IsCoverEnum.TRUE.getValue()));
        return ResultGenerator.genSuccessResult(PageResponse.<UploadFile>builder()
                .total(uploadFileListCo.getTotal())
                .list(fileList)
                .data(photoAlbum)
                .build());
    }

    /**
     * 相册列表
     * @return
     */
    @ApiVersion
    @ApiOperation(value = "文件列表", notes = "文件列表")
    @GetMapping("/v1/list")
    public Result listV1(@ModelAttribute UploadFileListV1Co uploadFileListV1Co) {
        uploadFileListV1Co.setTotal(uploadFileService.totalV1(uploadFileListV1Co));
        List<UploadFile> fileList = uploadFileService.listV1(uploadFileListV1Co);
        PhotoAlbum photoAlbum = photoAlbumService.get(uploadFileListV1Co.getAlbumId());
        fileList.stream()
                .filter(file -> file.getFileId().equals(photoAlbum.getCover()))
                .forEach(file -> file.setIsCover(IsCoverEnum.TRUE.getValue()));
        return ResultGenerator.genSuccessResult(PageResponse.<UploadFile>builder()
                .total(uploadFileListV1Co.getTotal())
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
        uploadFileService.download(fileId, response);
    }

    /**
     * 文件人脸信息列表
     * @param uploadFileFaceListCo
     * @return
     */
    @ApiOperation(value = "文件人脸信息列表", notes = "文件人脸信息列表")
    @GetMapping("/listFileFace")
    public Result listFileFace(UploadFileFaceListCo uploadFileFaceListCo) {
        return ResultGenerator.genSuccessResult(PageResponse.<UploadFileFace>builder()
                .total(uploadFileService.totalFileFace(uploadFileFaceListCo))
                .list(uploadFileService.listFileFace(uploadFileFaceListCo))
                .build());
    }
}
