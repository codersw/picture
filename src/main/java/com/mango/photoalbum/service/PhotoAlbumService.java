package com.mango.photoalbum.service;

import com.mango.photoalbum.model.co.PhotoAlbumCo;
import com.mango.photoalbum.model.vo.PhotoAlbumVo;

import java.util.List;

public interface PhotoAlbumService {

    PhotoAlbumVo save(PhotoAlbumCo PhotoAlbumCo);

    void delete(String albumId);

    PhotoAlbumVo get(String albumId);

    List<PhotoAlbumVo> list();
}
