Hugging Face's logo
---
language: 
- om
- am
- rw
- rn
- ha
- ig
- pcm
- so
- sw
- ti
- yo
- multilingual

---
# afriberta_base
## Model description
AfriBERTa base is a pretrained multilingual language model with around 111 million parameters.
The model has 8 layers, 6 attention heads, 768 hidden units and 3072 feed forward size.
The model was pretrained on 11 African languages namely - Afaan Oromoo (also called Oromo), Amharic, Gahuza (a mixed language containing Kinyarwanda and Kirundi), Hausa, Igbo, Nigerian Pidgin, Somali, Swahili, Tigrinya and Yorùbá.
The model has been shown to obtain competitive downstream performances on text classification and Named Entity Recognition on several African languages, including those it was not pretrained on. 


## Intended uses & limitations

#### How to use
You can use this model with Transformers for any downstream task. 
For example, assuming we want to finetune this model on a token classification task, we do the following:

```python
>>> from transformers import AutoTokenizer, AutoModelForTokenClassification
>>> model = AutoModelForTokenClassification.from_pretrained("castorini/afriberta_base")
>>> tokenizer = AutoTokenizer.from_pretrained("castorini/afriberta_base")
# we have to manually set the model max length because it is an imported sentencepiece model, which huggingface does not properly support right now
>>> tokenizer.model_max_length = 512 
```

#### Limitations and bias
- This model is possibly limited by its training dataset which are majorly obtained from news articles from a specific span of time. Thus, it may not generalize well.
- This model is trained on very little data (less than 1 GB), hence it may not have seen enough data to learn very complex linguistic relations.

## Training data
The model was trained on an aggregation of datasets from the BBC news website and Common Crawl. 

## Training procedure
For information on training procedures, please refer to the AfriBERTa [paper]() or [repository](https://github.com/keleog/afriberta)

### BibTeX entry and citation info
```
@inproceedings{ogueji-etal-2021-small,
    title = "Small Data? No Problem! Exploring the Viability of Pretrained Multilingual Language Models for Low-resourced Languages",
    author = "Ogueji, Kelechi  and
      Zhu, Yuxin  and
      Lin, Jimmy",
    booktitle = "Proceedings of the 1st Workshop on Multilingual Representation Learning",
    month = nov,
    year = "2021",
    address = "Punta Cana, Dominican Republic",
    publisher = "Association for Computational Linguistics",
    url = "https://aclanthology.org/2021.mrl-1.11",
    pages = "116--126",
}
```


