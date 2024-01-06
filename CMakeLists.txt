# Copyright (C) 2024 Devin Rockwell
# 
# This file is part of graphene.
# 
# graphene is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# graphene is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with graphene.  If not, see <http://www.gnu.org/licenses/>.

cmake_minimum_required(VERSION 3.16)

project(
    Graphene
    VERSION 0.1
    LANGUAGES C
)

add_executable(graphene src/main.c)

set_property(TARGET graphene PROPERTY C_STANDARD 90)

if(MSVC)
  target_compile_options(graphene PRIVATE /W4 /WX)
else()
  target_compile_options(graphene PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
