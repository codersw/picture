package com.mango.photoalbum.controller;


import com.mango.photoalbum.annotation.ApiVersion;
import com.mango.photoalbum.annotation.RequiredPermission;
import com.mango.photoalbum.constant.PermissionConst;
import com.mango.photoalbum.model.*;
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
    @RequiredPermission
    public Result get(@PathVariable String albumId) {
        return ResultGenerator.genSuccessResult(photoAlbumService.get(albumId));
    }

    /**
     * 相册详情
     * @param albumId
     * @return
     */
    @ApiOperation(value = "相册详情", notes = "相册详情")
    @GetMapping("/admin/{albumId}")
    @RequiredPermission(PermissionConst.SUPPERUSERFLAGENUM)
    public Result getAdmin(@PathVariable String albumId) {
        return ResultGenerator.genSuccessResult(photoAlbumService.get(albumId));
    }

    /**
     * 删除相册
     * @param albumId
     * @return
     */
    @ApiOperation(value = "删除相册", notes = "删除相册")
    @DeleteMapping("/admin/{albumId}")
    @RequiredPermission(PermissionConst.SUPPERUSERFLAGENUM)
    public Result deleteAdmin(@PathVariable String albumId) {
        photoAlbumService.delete(albumId);
        return ResultGenerator.genSuccessResult();
    }

    /**
     * 保存相册
     * @return
     */
    @ApiOperation(value = "保存相册", notes = "保存相册")
    @PostMapping("/admin")
    @RequiredPermission(PermissionConst.SUPPERUSERFLAGENUM)
    public Result saveAdmin(@RequestBody PhotoAlbumCo photoAlbumCo) {
        return ResultGenerator.genSuccessResult(photoAlbumService.save(photoAlbumCo));
    }

    /**
     * 保存相册
     * @return
     */
    @ApiOperation(value = "保存相册", notes = "保存相册")
    @PostMapping("/v1/admin")
    @RequiredPermission(PermissionConst.SUPPERUSERFLAGENUM)
    @ApiVersion
    public Result saveV1Admin(@RequestBody PhotoAlbumV1Co photoAlbumV1Co) {
        return ResultGenerator.genSuccessResult(photoAlbumService.saveV1(photoAlbumV1Co));
    }

    /**
     * 相册列表
     * @return
     */
    @ApiOperation(value = "相册列表", notes = "相册列表")
    @GetMapping("/list")
    @RequiredPermission
    public Result list(PhotoAlbumListCo photoAlbumListCo){
        return ResultGenerator.genSuccessResult(PageResponse.<PhotoAlbum>builder()
                .total(photoAlbumService.total(photoAlbumListCo))
                .list(photoAlbumService.list(photoAlbumListCo))
                .build());
    }

    /**
     * 相册列表
     * @return
     */
    @ApiOperation(value = "相册列表", notes = "相册列表")
    @GetMapping("/admin/list")
    @RequiredPermission(PermissionConst.SUPPERUSERFLAGENUM)
    public Result listAdmin(PhotoAlbumListCo photoAlbumListCo){
        return ResultGenerator.genSuccessResult(PageResponse.<PhotoAlbum>builder()
                .total(photoAlbumService.total(photoAlbumListCo))
                .list(photoAlbumService.list(photoAlbumListCo))
                .build());
    }

    /**
     * 相册列表
     * @return
     */
    @ApiOperation(value = "相册列表", notes = "相册列表")
    @GetMapping("/v1/list")
    @RequiredPermission
    @ApiVersion
    public Result v1list(PhotoAlbumListV1Co photoAlbumListV1Co){
        return ResultGenerator.genSuccessResult(PageResponse.<PhotoAlbum>builder()
                .total(photoAlbumService.totalV1(photoAlbumListV1Co))
                .list(photoAlbumService.listV1(photoAlbumListV1Co))
                .build());
    }

    /**
     * 相册列表
     * @return
     */
    @ApiOperation(value = "相册列表", notes = "相册列表")
    @GetMapping("/v1/admin/list")
    @ApiVersion
    @RequiredPermission(PermissionConst.SUPPERUSERFLAGENUM)
    public Result v1listAdmin(PhotoAlbumListV1Co photoAlbumListV1Co){
        return ResultGenerator.genSuccessResult(PageResponse.<PhotoAlbum>builder()
                .total(photoAlbumService.totalAdmin(photoAlbumListV1Co))
                .list(photoAlbumService.listAdmin(photoAlbumListV1Co))
                .build());
    }
}
