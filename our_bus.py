#!/usr/bin/env python
# coding: utf-8

# # Required Libraries

# In[1]:


import pandas as pd
import numpy as np 
import matplotlib.pyplot as plt
import seaborn as sns


# # Reading the sample data

# In[2]:


data = pd.read_csv("sample_data.csv")


# In[3]:


#Head of the dataset

data.head()


# In[4]:


#The data types of the dataset

data.dtypes


# In[5]:


#Checking for null values in the dataset

data.isna().sum()


# Before further analysis, let us take a look at the routes and bus start and stop numbers

# In[6]:


print("The route numbers that are being followed by the buses are {}".format(data.route_no.unique()))
print(100*'-')
print("The buses from where they start, the stop numbers are {}".format(data.from_stop.unique()))
print(100*'-')
print("The buses where the journey ends, the stop numbers are {}".format(data.to_stop.unique()))


# ###  Since the order_date, departure_date are being treated here as object, we will change it to datetime dtype.
# ### route_no, from_stop, to_stop here are int64 dtype we will change the dtype and treat as categorical. 

# In[7]:


data['order_date'] = pd.to_datetime(data['order_date'])


# In[8]:


data['travel_date'] = pd.to_datetime(data['travel_date'])


# In[9]:


data['route_no'] = data['route_no'].astype('object')


# In[10]:


data['from_stop'] = data['from_stop'].astype('object')


# In[11]:


data['to_stop'] = data['to_stop'].astype('object')


# In[12]:


data.dtypes


# In[13]:


data.head()


# ## Let us see which route is most plyed upon

# In[14]:


plt.figure(figsize=(15,9))
sns.countplot(x = 'route_no',data=data)
plt.xlabel('route_no id')


# Route 63916 is the most plyed closely followed by 63912, route number 226556 is the least followed upon 

# ## Let us see which is the stop where people get on the bus

# In[15]:


plt.figure(figsize=(15,9))
sns.countplot(x = 'from_stop', data=data)
plt.xlabel('from_stop id')


#  2655 is the bus stop most of the commuters start their journey from 

# ## Now let us see to which route is the where the most journeys end

# In[16]:


plt.figure(figsize=(15,9))
sns.countplot(x = 'to_stop',data=data)
plt.xlabel('to_stop id')


# to_stop id 2790 is the most deboarded point

# ### From the above plots we can see that from_stop id 9450 is the only point which is not present in the to_stop.
# ### this could be because of the lack of the data points in the dataset or the from_stop id 9450 is not being used in return journey. 
# ### Similarly for to_stop 2925 where passengers deboard. 

# # Feature Engineering

# In[17]:


data['departure_time'] = pd.to_datetime(data['departure_time'], format='%H:%M:%S')
data['departure_hour'] = data.departure_time.apply(lambda x: x.hour)
data['departure_hour'] = data['departure_hour'].astype('object')


# In[18]:


data["order_day"] = data["order_date"].dt.dayofweek
data["order_day"] = data["order_day"].astype('object')


# In[19]:


data["travel_day"] = data["travel_date"].dt.dayofweek
data["travel_day"] = data["travel_day"].astype('object')


# In[20]:


data["order_month"] = data["order_date"].dt.month
data["order_month"] = data["order_month"].astype('object')


# In[21]:


data["travel_month"] = data["travel_date"].dt.month
data["travel_month"] = data["travel_month"].astype('object')


# In[22]:


data.head()


# In[23]:


data.dtypes


# In[24]:


data.head()


# ## Let us look at the distribution of the days where the order is being booked

# In[25]:


plt.figure(figsize=(15,9))
sns.countplot(x = "order_day",data = data)


# ### There is no clear difference in the distribution on the order day

# ## Now let us take a look at the travel day 

# In[26]:


plt.figure(figsize=(15,9))
sns.countplot(x = 'travel_day',data = data)


# ### Travelling customers increases periodically as they close to the weekends

# ## Now the travelling month distribution

# In[27]:


plt.figure(figsize=(15,9))
sns.countplot(x = 'travel_month',data=data)


# ## June has the highest volume in the travelling influx

# In[28]:


# Params get plot bigger
plt.rcParams["axes.labelsize"] = 16
plt.rcParams["xtick.labelsize"] = 14
plt.rcParams["ytick.labelsize"] = 14
plt.rcParams["legend.fontsize"] = 12
plt.rcParams["figure.figsize"] = [15, 7]


# In[29]:


data['COUNTER'] = 1


# In[30]:


group_data = data.groupby(['route_no','travel_month']).sum()[['COUNTER']].plot.bar(rot = 90) # If you want to rotate labels from x axis
_ = group_data.set(xlabel = 'route_no,travel_month', ylabel = 'count') 


# ## route_no 190808 in the month June is the most plyed upon

# In[31]:


group_data = data.groupby(['from_stop','travel_month']).sum()[['COUNTER']].plot.bar(rot = 90) 
_ = group_data.set(xlabel = 'from_stop,travel_month', ylabel = 'count') 


# ## from_stop id 2655, in the month of june passengers board the buses.

# In[32]:


group_data = data.groupby(['to_stop','travel_month']).sum()[['COUNTER']].plot.bar(rot = 90) 
_ = group_data.set(xlabel = 'to_stop,travel_month', ylabel = 'count') 


# In[33]:


group_data = data.groupby(['route_no','travel_day']).sum()[['COUNTER']].plot.bar(rot = 90) 
_ = group_data.set(xlabel = 'route_no,travel_day', ylabel = 'count') 


# ### route_no id 8192 on the thursday is the most plyed upon

# ## again to_stop id 2655 in the month of June, the people are stopping there. (Strange!)

# In[34]:


from datetime import datetime


# In[35]:


data['difference'] = (data['order_date'] - data['travel_date']).abs().dt.days


# In[36]:


data['difference'].plot.density(bw_method = 0.3)


# ### The distribution of the difference is right skewed in nature

# In[ ]:




