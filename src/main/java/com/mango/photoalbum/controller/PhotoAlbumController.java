package com.mango.photoalbum.controller;

import com.mango.photoalbum.model.PhotoAlbumCo;
import com.mango.photoalbum.result.Result;
import com.mango.photoalbum.result.ResultGenerator;
import com.mango.photoalbum.service.PhotoAlbumService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;

/**
 * 文件接口
 * @author swen
 */
@Api(value = "相册接口", tags = {"相册接口"})
@Slf4j
@RestController
@RequestMapping("/album")
public class PhotoAlbumController {

    @Resource
    private PhotoAlbumService photoAlbumService;

    /**
     * 相册详情
     * @param albumId
     * @return
     */
    @ApiOperation(value = "相册详情", notes = "相册详情")
    @GetMapping("/{albumId}")
    public Result get(@PathVariable String albumId) {
        return ResultGenerator.genSuccessResult(photoAlbumService.get(albumId));
    }

    /**
     * 删除相册
     * @param albumId
     * @return
     */
    @ApiOperation(value = "删除相册", notes = "删除相册")
    @DeleteMapping("/{albumId}")
    public Result delete(@PathVariable String albumId) {
        photoAlbumService.delete(albumId);
        return ResultGenerator.genSuccessResult();
    }

    /**
     * 保存相册
     * @return
     */
    @ApiOperation(value = "保存相册", notes = "保存相册")
    @PostMapping
    public Result save(PhotoAlbumCo photoAlbumCo) {
        return ResultGenerator.genSuccessResult(photoAlbumService.save(photoAlbumCo));
    }

    /**
     *
     * @return
     */
    @ApiOperation(value = "相册列表", notes = "相册列表")
    @GetMapping("/list")
    public Result list(){
        return null;
    }
}
