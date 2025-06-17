* `RenderPass` - defines how to render the frame once; renders to a given `Framebuffer`; may contain multiple `RenderSubPass`es
* `RenderSubPass` - defines how to render multiple `Drawable`s; uses a single `ShaderProgram` to render them all; might refer to existing `Framebuffer`s as inputs, filled by previous `RenderPass`es; optionally fills out the `Uniform`s of a `ShaderProgram` for rendering
* `Framebuffer` - contains multiple `Texture` objects, where the frame will be rendered to
* `Drawable` - encapsulates a list of `VAO`s and defines a method to update them on each frame (?) if needed; optionally fills out the `Uniform`s of a `ShaderProgram` for rendering (textures, for instance)
* `VAO` -  (aka `VertexAttributeObject`) encapsulates a list of one or multiple **named** vertex attributes (position, normal, texture coordinates etc.), describing each specific _vertex_ that is rendered
* `Uniform` - describes a single `uniform` variable within `ShaderProgram`; could be a `UniformBufferObject` storing multiple values or an simple `Uniform` storing single value
* `ShaderProgram` - encapsulates one or more `ShaderStage`s - `VertexShader`, `FragmentShader`, `GeometryShader`, etc.

```cpp
class MyModel1 : public Drawable
{
public:
    MyModel1() : Drawable() {}

    void load() override
    {
        auto scene = AssimpLoader::fromFile("something.dae");
        auto mesh = scene->getMesh("root");

        m_vao = this->createDynamicVertexAttributeObject()
            ->elements(mesh->getVertexData()->indices)
            ->attribute<glm::vec3>("position", mesh->getVertexData()->positions) // creates a separate Buffer and fills it with data; possible template type inference
            ->attribute<glm::vec3>("normal", mesh->getVertexData()->normals)
            ->attribute<glm::vec2>("textureCoord", mesh->getVertexData()->textureCoordinates)
            ->build();

        m_ubo = this->createUniformObject()
            ->attribute("transformation", m_transformation)
            ->build();

        m_diffuseTexture = mesh->getTextureData()->diffuse;
        m_specularMap = mesh->getTextureData()->specular;
        m_normalMap = mesh->getTextureData()->normal;
        m_emissionMap = mesh->getTextureData()->emission;
    }

    // TODO: can not be an abstract ShaderProgram to ensure interface for setting uniforms and vertex attributes? make specific interface instead at compile time?
    void bind(ShaderProgram shader) override
    {
        // m_diffuseMap->bind(); // calls to makeResident() underneath
        // m_normalMap->bind();

        shader->setUniform("objectData", m_ubo); // calls to bindBase() underneath
        shader->setUniform("texture", m_diffuseMap); // template param inference; calls to handle() underneath
        shader->setUniform("normalMap", m_normalMap); // calls m_normalMap->bind() underneath on Drawable::bind() and m_normalMap->unbind() on Drawable::unbind() automatically; works similar to locks - if bindCount == 0 then calls bind(); anyways increments bindCount; adds m_diffuseMap to a list of bound resources
    }

    void unbind(ShaderProgram shader) override
    {
        // m_diffuseMap->unbind();
        // m_normalMap->unbind();

        // iterates over a list of bound resources and decrements bindCount; if bindCount == 0 then calls resource->unbind(); calls resource->makeNonResident() for textures underneath
    }

    void draw(ShaderProgram shader, float deltaTime) override
    {
        m_vao->drawElements(GL_TRIANGLES); // uses the element type and element count from ->elements() call from load()
    }

private:
    VAO* m_vao;
    UBO* m_ubo;

    Texture* m_diffuseTexture;
    Texture* m_specularMap;
    Texture* m_normalMap;
    Texture* m_emissionMap;
};

// TODO: what if a sub-pass is just a function?
namespace v1
{
    class ShadowMappingSubPass : public RenderSubPass
    {
    public:
        // TODO: generate a UID for this class so later it could be used for loading data from disk cache (shaders, for example)
        ShadowMappingSubPass() : RenderSubPass() {}

        void load() override
        {
            m_shader = this->createShaderProgram()
                ->vertexShader(Shader::fromFile("media/shadow-mapping.vert"))
                ->fragmentShader(Shader::fromFile("media/shadow-mapping.frag"))
                ->build(); // TODO: #ifndef _DEBUG compile and store shader on disk
        }

        // default implementation, no need to override, really
        void draw(std::vector<Drawable*> drawables, float deltaTime) override
        {
            m_shader->bind();

            for (auto drawable : drawables)
            {
                drawable->bind(m_shader);
                drawable->draw(m_shader, deltaTime);
                drawable->unbind(m_shader);
            }

            m_shader->unbind();
        }

    private:
        ShaderProgram* m_shader;
    };

    class ShadowMappingPass : public RenderPass
    {
    public:
        ShadowMappingPass(Framebuffer* shadowMapFramebuf) : RenderPass(), m_shadowMapFramebuf(shadowMapFramebuf) {}

        void load() override
        {
            this->addRenderSubPass<ShadowMappingSubPass>();
        }

        // default implementation, no need to override, really
        void draw(std::vector<Drawable*> drawables, float deltaTime) override
        {
            m_shadowMapFramebuf->bind();

            for (auto subPass : this->subPasses)
            {
                subPass->draw(drawables, deltaTime);
            }

            m_shadowMapFramebuf->unbind();
        }

    private:
        Framebuffer* m_shadowMapFramebuf;
    };

    class EmissiveRenderingSubPass : public RenderSubPass
    {
    public:
        EmissiveRenderingSubPass() : RenderSubPass() {}

        void load() override
        {
            m_shader = this->createShaderProgram()
                ->vertexShader(Shader::fromFile("media/emissive-rendering.vert"))
                ->fragmentShader(Shader::fromFile("media/emissive-rendering.frag"))
                ->build(); // TODO: #ifndef _DEBUG compile and store shader on disk
        }

    private:
        ShaderProgram* m_shader;
    };

    class EmissiveRenderingPass : public RenderPass
    {
    public:
        EmissiveRenderingPass() : RenderPass() {}

        void load() override
        {
            this->addRenderSubPass<EmissiveRenderingSubPass>();
        }

        void draw() override
        {
            for (auto i = 0; i < 10; ++i)
            {
                // blur vertically and then horizontally
                if (i % 2 == 0)
                {
                    m_framebuf1->bind();
                } else
                {
                    m_framebuf2->bind();
                }

                for (auto subPass : this->subPasses)
                {
                    subPass->draw(drawables, deltaTime);
                }

                if (i % 2 == 0)
                {
                    m_framebuf1->unbind();
                } else
                {
                    m_framebuf2->unbind();
                }
            }
        }

    private:
        Framebuffer* m_framebuf1;
        Framebuffer* m_framebuf2;
    };
}

namespace v2
{
    class ShadowMappingPass : public RenderPass
    {
    public:
        ShadowMappingPass(Framebuffer* shadowMapFramebuf) : RenderPass(), m_shadowMapFramebuf(shadowMapFramebuf) {}

        void load() override
        {
            m_shader = this->createShaderProgram()
                ->vertexShader(Shader::fromFile("media/shadow-mapping.vert"))
                ->fragmentShader(Shader::fromFile("media/shadow-mapping.frag"))
                ->build(); // TODO: #ifndef _DEBUG compile and store shader on disk
        }

        // TODO: how to pass extra params to the shader (like uniforms, MVP matrices, etc.)?
        // idea #1: use setters and getters in each RenderPass object
        // idea #2: use global ApplicationState object and pass it around
        void draw(std::vector<Drawable*> drawables, float deltaTime) override
        {
            m_shadowMapFramebuf->bind();

            subPass1(drawables, deltaTime);

            m_shadowMapFramebuf->unbind();
        }

        void subPass1(std::vector<Drawable*> drawables, float deltaTime)
        {
            m_shader->bind();

            for (auto drawable : drawables)
            {
                drawable->bind(m_shader);
                drawable->draw(m_shader, deltaTime);
                drawable->unbind(m_shader);
            }

            m_shader->unbind();
        }

    private:
        Framebuffer* m_shadowMapFramebuf;
        ShaderProgram* m_shader;
    };

    class EmissiveRenderingPass : public RenderPass
    {
    public:
        EmissiveRenderingPass() : RenderPass() {}

        void load() override
        {
            m_blurShader = this->createShaderProgram()
                ->vertexShader(Shader::fromFile("media/blur.vert"))
                ->fragmentShader(Shader::fromFile("media/blur.frag"))
                ->build();
        }

        void draw(std::vector<Drawable*> drawables, float deltaTime) override
        {
            m_blurShader->bind();

            for (auto i = 0; i < 10; ++i)
            {
                // blur vertically and then horizontally
                if (i % 2 == 0)
                {
                    m_framebuf1->bindToRead();
                    m_framebuf2->bindToWrite();

                    blurPass(drawables, deltaTime, true);

                    m_framebuf1->unbind();
                    m_framebuf2->unbind();
                } else
                {
                    m_framebuf2->bindToRead();
                    m_framebuf1->bindToWrite();

                    blurPass(drawables, deltaTime, false);

                    m_framebuf1->unbind();
                    m_framebuf2->unbind();
                }
            }

            m_blurShader->unbind();
        }

        void blurPass(std::vector<Drawable*> drawables, float deltaTime, bool isVerticalBlur)
        {
            m_blurShader->setUniform("isVerticalBlur", isVerticalBlur);

            for (auto drawable : drawables)
            {
                drawable->bind(m_blurShader);
                drawable->draw(m_blurShader, deltaTime);
                drawable->unbind(m_blurShader);
            }
        }

    private:
        Framebuffer* m_framebuf1;
        Framebuffer* m_framebuf2;
        ShaderProgram* m_blurShader;
    };
}
```
